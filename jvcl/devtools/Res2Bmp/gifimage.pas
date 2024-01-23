unit GIFImage;
////////////////////////////////////////////////////////////////////////////////
// Changed 2001.07.23 by Finn Tolderlund
// Changed according to e-mail from "Rolf Frei" <rolf@eicom.ch>
//   on 2001.07.23 so that it works in Delphi 6.
//
//                                                                            //
// Project:	GIF Graphics Object                                           //
// Module:	gifimage                                                      //
// Description:	TGraphic implementation of the GIF89a graphics format         //
// Version:	2.2                                                           //
// Release:	5                                                             //
// Date:	23-MAY-1999                                                   //
// Target:	Win32, Delphi 2, 3, 4 & 5, C++ Builder 3 & 4                  //
// Author(s):	anme: Anders Melander, anders@melander.dk                     //
//		fila: Filip Larsen                                            //
//		rps: Reinier Sterkenburg                                      //
// Copyright:	(c) 1997-99 Anders Melander.                                  //
//		All rights reserved.                                          //
// Formatting:	2 space indent, 8 space tabs, 80 columns.                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Please read the "Conditions of use" in the release notes.                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
// Known problems:
//
// * The combination of buffered, tiled and transparent draw will display the
//   background incorrectly (scaled).
//   If this is a problem for you, use non-buffered (goDirectDraw) drawing
//   instead.
//
// * The combination of non-buffered, transparent and stretched draw is
//   sometimes distorted with a pattern effect when the image is displayed
//   smaller than the real size (shrinked).
//
// * Buffered display flickers when TGIFImage is used by a transparent TImage
//   component.
//   This is a problem with TImage caused by the fact that TImage was designed
//   with static images in mind. Not much I can do about it.
//
////////////////////////////////////////////////////////////////////////////////
// To do (in rough order of priority):
// { TODO -oanme -cFeature : TImage hook for destroy notification. }
// { TODO -oanme -cFeature : TBitmap pool to limit resource consumption on Win95/98. }
// { TODO -oanme -cImprovement : Make BitsPerPixel property writable. }
// { TODO -oanme -cFeature : Visual GIF component. }
// { TODO -oanme -cImprovement : Easier method to determine DrawPainter status. }
// { TODO -oanme -cFeature : Import to 256+ color GIF. }
// { TODO -oanme -cFeature : Make some of TGIFImage's properties persistent (DrawOptions etc). }
// { TODO -oanme -cFeature : Add TGIFImage.Persistent property. Should save published properties in application extension when this options is set. }
// { TODO -oanme -cBugFix : Solution for background buffering in scrollbox. }
//
//////////////////////////////////////////////////////////////////////////////////
{$WARN SYMBOL_PLATFORM OFF}
{$ifdef BCB}
{$ObjExportAll On}
{$endif}

interface
////////////////////////////////////////////////////////////////////////////////
//
//		Conditional Compiler Symbols
//
////////////////////////////////////////////////////////////////////////////////
(*
  DEBUG				Must be defined if any of the DEBUG_xxx
  				symbols are defined.
                                If the symbol is defined the source will not be
                                optimized and overflow- and range checks will be
                                enabled.

  DEBUG_HASHPERFORMANCE		Calculates hash table performance data.
  DEBUG_HASHFILLFACTOR		Calculates fill factor of hash table -
  				Interferes with DEBUG_HASHPERFORMANCE.
  DEBUG_COMPRESSPERFORMANCE	Calculates LZW compressor performance data.
  DEBUG_DECOMPRESSPERFORMANCE	Calculates LZW decompressor performance data.
  DEBUG_DITHERPERFORMANCE	Calculates color reduction performance data.
  DEBUG_DRAWPERFORMANCE		Calculates low level drawing performance data.
  				The performance data for DEBUG_DRAWPERFORMANCE
                                will be displayed when you press the Ctrl key.
  DEBUG_RENDERPERFORMANCE	Calculates performance data for the GIF to
  				bitmap converter.
  				The performance data for DEBUG_DRAWPERFORMANCE
                                will be displayed when you press the Ctrl key.

  GIF_NOSAFETY			Define this symbol to disable overflow- and
				range checks.
                                Ignored if the DEBUG symbol is defined.

  STRICT_MOZILLA		Define to mimic Mozilla as closely as possible.
  				If not defined, a slightly more "optimal"
                                implementation is used (IMHO).

  FAST_AS_HELL			Define this symbol to use strictly GIF compliant
  				(but too fast) animation timing.
                                Since our paint routines are much faster and
                                more precise timed than Mozilla's, the standard
                                GIF and Mozilla values causes animations to loop
                                faster than they would in Mozilla.
                                If the symbol is _not_ defined, an alternative
                                set of tweaked timing values will be used.
                                The tweaked values are not optimal but are based
                                on tests performed on my reference system:
                                - Windows 95
                                - 133 MHz Pentium
                                - 64Mb RAM
                                - Diamond Stealth64/V3000
                                - 1600*1200 in 256 colors
                                The alternate values can be modified if you are
                                not satisfied with my defaults (they can be
                                found a few pages down).

  REGISTER_TGIFIMAGE            Define this symbol to register TGIFImage with
  				the TPicture class and integrate with TImage.
                                This is required to be able to display GIFs in
                                the TImage component.
                                The symbol is defined by default.
                                Undefine if you use another GIF library to
                                provide GIF support for TImage.

  PIXELFORMAT_TOO_SLOW		When this symbol is defined, the internal
  				PixelFormat routines are used in some places
                                instead of TBitmap.PixelFormat.
                                The current implementation (Delphi4, Builder 3)
                                of TBitmap.PixelFormat can in some situation
                                degrade performance.
                                The symbol is defined by default.

  CREATEDIBSECTION_SLOW		If this symbol is defined, TDIBWriter will
  				use global memory as scanline storage, instead
                                of a DIB section.
                                Benchmarks have shown that a DIB section is
                                twice as slow as global memory.
                                The symbol is defined by default.
                                The symbol requires that PIXELFORMAT_TOO_SLOW
                                is defined.

  SERIALIZE_RENDER		Define this symbol to serialize threaded
  				GIF to bitmap rendering.
                                When a GIF is displayed with the goAsync option
                                (the default), the GIF to bitmap rendering is
                                executed in the context of the draw thread.
                                If more than one thread is drawing the same GIF
                                or the GIF is being modified while it is
                                animating, the GIF to bitmap rendering should be
                                serialized to guarantee that the bitmap isn't
                                modified by more than one thread at a time. If
                                SERIALIZE_RENDER is defined, the draw threads
                                uses TThread.Synchronize to serialize GIF to
                                bitmap rendering.
*)

{$DEFINE REGISTER_TGIFIMAGE}
{$DEFINE PIXELFORMAT_TOO_SLOW}
{$DEFINE CREATEDIBSECTION_SLOW}

////////////////////////////////////////////////////////////////////////////////
//
//		Determine Delphi and C++ Builder version
//
////////////////////////////////////////////////////////////////////////////////

// Delphi 1.x
{$IFDEF VER80}
  'Error: TGIFImage does not support Delphi 1.x'
{$ENDIF}

// Delphi 2.x
{$IFDEF VER90}
  {$DEFINE VER9x}
{$ENDIF}

// C++ Builder 1.x
{$IFDEF VER93}
  // Good luck...
  {$DEFINE VER9x}
{$ENDIF}

// Delphi 3.x
{$IFDEF VER100}
  {$DEFINE VER10_PLUS}
  {$DEFINE D3_BCB3}
{$ENDIF}

// C++ Builder 3.x
{$IFDEF VER110}
  {$DEFINE VER10_PLUS}
  {$DEFINE VER11_PLUS}
  {$DEFINE D3_BCB3}
  {$DEFINE BAD_STACK_ALIGNMENT}
{$ENDIF}

// Delphi 4.x
{$IFDEF VER120}
  {$DEFINE VER10_PLUS}
  {$DEFINE VER11_PLUS}
  {$DEFINE VER12_PLUS}
  {$DEFINE BAD_STACK_ALIGNMENT}
{$ENDIF}

// C++ Builder 4.x
{$IFDEF VER125}
  {$DEFINE VER10_PLUS}
  {$DEFINE VER11_PLUS}
  {$DEFINE VER12_PLUS}
  {$DEFINE VER125_PLUS}
  {$DEFINE BAD_STACK_ALIGNMENT}
{$ENDIF}

// Delphi 5.x
{$IFDEF VER130}
  {$DEFINE VER10_PLUS}
  {$DEFINE VER11_PLUS}
  {$DEFINE VER12_PLUS}
  {$DEFINE VER125_PLUS}
  {$DEFINE VER13_PLUS}
  {$DEFINE BAD_STACK_ALIGNMENT}
{$ENDIF}

// Delphi 6.x
{$IFDEF VER140}
{$WARN SYMBOL_PLATFORM OFF}
  {$DEFINE VER10_PLUS}
  {$DEFINE VER11_PLUS}
  {$DEFINE VER12_PLUS}
  {$DEFINE VER125_PLUS}
  {$DEFINE VER13_PLUS}
  {$DEFINE VER14_PLUS}
  {$DEFINE BAD_STACK_ALIGNMENT}
{$ENDIF}

{$IFDEF VER150}
{$WARN SYMBOL_PLATFORM OFF}
  {$DEFINE VER10_PLUS}
  {$DEFINE VER11_PLUS}
  {$DEFINE VER12_PLUS}
  {$DEFINE VER125_PLUS}
  {$DEFINE VER13_PLUS}
  {$DEFINE VER14_PLUS}
  {$DEFINE BAD_STACK_ALIGNMENT}
{$ENDIF}

// Unknown compiler version - assume D4 compatible
{$IFNDEF VER9x}
  {$IFNDEF VER10_PLUS}
    {$DEFINE VER10_PLUS}
    {$DEFINE VER11_PLUS}
    {$DEFINE VER12_PLUS}
    {$DEFINE BAD_STACK_ALIGNMENT}
  {$ENDIF}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
//		Compiler Options required to compile this library
//
////////////////////////////////////////////////////////////////////////////////
{$A+,B-,H+,J+,K-,M-,T-,X+}

// Debug control - You can safely change these settings
{$IFDEF DEBUG}
  {$C+}	// ASSERTIONS
  {$O-}	// OPTIMIZATION
  {$Q+}	// OVERFLOWCHECKS
  {$R+}	// RANGECHECKS
{$ELSE}
  {$C-}	// ASSERTIONS
  {$IFDEF GIF_NOSAFETY}
    {$Q-}// OVERFLOWCHECKS
    {$R-}// RANGECHECKS
  {$ENDIF}
{$ENDIF}

// Special options for Time2Help parser
{$ifdef TIME2HELP}
{$UNDEF PIXELFORMAT_TOO_SLOW}
{$endif}

////////////////////////////////////////////////////////////////////////////////
//
//			External dependecies
//
////////////////////////////////////////////////////////////////////////////////
uses
  sysutils,
  Windows,
  Graphics,
  Classes;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFImage library version
//
////////////////////////////////////////////////////////////////////////////////
const
  GIFVersion		= $0202;
  GIFVersionMajor	= 2;
  GIFVersionMinor	= 2;
  GIFVersionRelease	= 5;

////////////////////////////////////////////////////////////////////////////////
//
//			Misc constants and support types
//
////////////////////////////////////////////////////////////////////////////////
const
  GIFMaxColors	= 256;			// Max number of colors supported by GIF
  					// Don't bother changing this value!

  BitmapAllocationThreshold = 500000;	// Bitmap pixel count limit at which
  					// a newly allocated bitmap will be
                                        // converted to 1 bit format before
                                        // being resized and converted to 8 bit.

var
{$IFDEF FAST_AS_HELL}
  GIFDelayExp: integer = 10;		// Delay multiplier in mS.
{$ELSE}
  GIFDelayExp: integer = 12;		// Delay multiplier in mS. Tweaked.
{$ENDIF}
					// * GIFDelayExp:
  					// The following delay values should all
                                        // be multiplied by this value to
                                        // calculate the effective time (in mS).
                                        // According to the GIF specs, this
                                        // value should be 10.
                                        // Since our paint routines are much
                                        // faster than Mozilla's, you might need
                                        // to increase this value if your
                                        // animations loops too fast. The
                                        // optimal value is impossible to
                                        // determine since it depends on the
                                        // speed of the CPU, the viceo card,
                                        // memory and many other factors.

  GIFDefaultDelay: integer = 10;	// * GIFDefaultDelay:
  					// Default animation delay.
  					// This value is used if no GCE is
                                        // defined.
                                        // (10 = 100 mS)

{$IFDEF FAST_AS_HELL}
  GIFMinimumDelay: integer = 1;		// Minimum delay (from Mozilla source).
  					// (1 = 10 mS)
{$ELSE}
  GIFMinimumDelay: integer = 3;		// Minimum delay - Tweaked.
{$ENDIF}
					// * GIFMinimumDelay:
					// The minimum delay used in the Mozilla
                                        // source is 10mS. This corresponds to a
                                        // value of 1. However, since our paint
                                        // routines are much faster than
                                        // Mozilla's, a value of 3 or 4 gives
                                        // better results.

  GIFMaximumDelay: integer = 1000;	// * GIFMaximumDelay:
  					// Maximum delay when painter is running
  					// in main thread (goAsync is not set).
                                        // This value guarantees that a very
                                        // long and slow GIF does not hang the
                                        // system.
                                        // (1000 = 10000 mS = 10 Seconds)

type
  TGIFVersion = (gvUnknown, gv87a, gv89a);
  TGIFVersionRec = array[0..2] of char;

const
  GIFVersions : array[gv87a..gv89a] of TGIFVersionRec = ('87a', '89a');

type
  // TGIFImage mostly throws exceptions of type GIFException
  GIFException = class(EInvalidGraphic);

  // Severity level as indicated in the Warning methods and the OnWarning event
  TGIFSeverity = (gsInfo, gsWarning, gsError);

////////////////////////////////////////////////////////////////////////////////
//
//			Delphi 2.x support
//
////////////////////////////////////////////////////////////////////////////////
{$IFDEF VER9x}
// Delphi 2 doesn't support TBitmap.PixelFormat
{$DEFINE PIXELFORMAT_TOO_SLOW}
type
  // TThreadList from Delphi 3 classes.pas
  TThreadList = class
  private
    FList: TList;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: Pointer);
    procedure Clear;
    function  LockList: TList;
    procedure Remove(Item: Pointer);
    procedure UnlockList;
  end;

  // From Delphi 3 sysutils.pas
  EOutOfMemory = class(Exception);

  // From Delphi 3 classes.pas
  EOutOfResources = class(EOutOfMemory);

  // From Delphi 3 windows.pas
  PMaxLogPalette = ^TMaxLogPalette;
  TMaxLogPalette = packed record
    palVersion: Word;
    palNumEntries: Word;
    palPalEntry: array [Byte] of TPaletteEntry;
  end; { TMaxLogPalette }

  // From Delphi 3 graphics.pas. Used by the D3 TGraphic class.
  TProgressStage = (psStarting, psRunning, psEnding);
  TProgressEvent = procedure (Sender: TObject; Stage: TProgressStage;
    PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string) of object;

  // From Delphi 3 windows.pas
  PRGBTriple = ^TRGBTriple;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
//			Forward declarations
//
////////////////////////////////////////////////////////////////////////////////
type
  TGIFImage = class;
  TGIFSubImage = class;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFItem
//
////////////////////////////////////////////////////////////////////////////////
  TGIFItem = class(TPersistent)
  private
    FGIFImage: TGIFImage;
  protected
    function GetVersion: TGIFVersion; virtual;
    procedure Warning(Severity: TGIFSeverity; Message: string); virtual;
  public
    constructor Create(GIFImage: TGIFImage); virtual;

    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToFile(const Filename: string); virtual;
    procedure LoadFromFile(const Filename: string); virtual;
    property Version: TGIFVersion read GetVersion;
    property Image: TGIFImage read FGIFImage;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFList
//
////////////////////////////////////////////////////////////////////////////////
  TGIFList = class(TPersistent)
  private
    FItems: TList;
    FImage: TGIFImage;
  protected
    function GetItem(Index: Integer): TGIFItem;
    procedure SetItem(Index: Integer; Item: TGIFItem);
    function GetCount: Integer;
    procedure Warning(Severity: TGIFSeverity; Message: string); virtual;
  public
    constructor Create(Image: TGIFImage);
    destructor Destroy; override;

    function Add(Item: TGIFItem): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function First: TGIFItem;
    function IndexOf(Item: TGIFItem): Integer;
    procedure Insert(Index: Integer; Item: TGIFItem);
    function Last: TGIFItem;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: TGIFItem): Integer;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromStream(Stream: TStream; Parent: TObject); virtual; abstract;

    property Items[Index: Integer]: TGIFItem read GetItem write SetItem; default;
    property Count: Integer read GetCount;
    property List: TList read FItems;
    property Image: TGIFImage read FImage;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFColorMap
//
////////////////////////////////////////////////////////////////////////////////
  // One way to do it:
  //  TBaseColor = (bcRed, bcGreen, bcBlue);
  //  TGIFColor = array[bcRed..bcBlue] of BYTE;
  // Another way:
  TGIFColor = packed record
    Red: byte;
    Green: byte;
    Blue: byte;
  end;

  TColorMap = packed array[0..GIFMaxColors-1] of TGIFColor;
  PColorMap = ^TColorMap;

  TUsageCount = record
    Count		: integer;	// # of pixels using color index
    Index		: integer;	// Color index
  end;
  TColormapHistogram = array[0..255] of TUsageCount;
  TColormapReverse = array[0..255] of byte;

  TGIFColorMap = class(TPersistent)
  private
    FColorMap		: PColorMap;
    FCount		: integer;
    FCapacity		: integer;
    FOptimized		: boolean;
  protected
    function GetColor(Index: integer): TColor;
    procedure SetColor(Index: integer; Value: TColor);
    function GetBitsPerPixel: integer;
    function DoOptimize: boolean;
    procedure SetCapacity(Size: integer);
    procedure Warning(Severity: TGIFSeverity; Message: string); virtual; abstract;
    procedure BuildHistogram(var Histogram: TColormapHistogram); virtual; abstract;
    procedure MapImages(var Map: TColormapReverse); virtual; abstract;

  public
    constructor Create;
    destructor Destroy; override;
    class function Color2RGB(Color: TColor): TGIFColor;
    class function RGB2Color(Color: TGIFColor): TColor;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream; Count: integer);
    procedure Assign(Source: TPersistent); override;
    function IndexOf(Color: TColor): integer;
    function Add(Color: TColor): integer;
    function AddUnique(Color: TColor): integer;
    procedure Delete(Index: integer);
    procedure Clear;
    function Optimize: boolean; virtual; abstract;
    procedure Changed; virtual; abstract;
    procedure ImportPalette(Palette: HPalette);
    procedure ImportColorTable(Pal: pointer; Count: integer);
    procedure ImportDIBColors(Handle: HDC);
    procedure ImportColorMap(Map: TColorMap; Count: integer);
    function ExportPalette: HPalette;
    property Colors[Index: integer]: TColor read GetColor write SetColor; default;
    property Data: PColorMap read FColorMap;
    property Count: integer read FCount;
    property Optimized: boolean read FOptimized write FOptimized;
    property BitsPerPixel: integer read GetBitsPerPixel;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFHeader
//
////////////////////////////////////////////////////////////////////////////////
  TLogicalScreenDescriptor = packed record
    ScreenWidth: word;              { logical screen width }
    ScreenHeight: word;             { logical screen height }
    PackedFields: byte;             { packed fields }
    BackgroundColorIndex: byte;     { index to global color table }
    AspectRatio: byte;              { actual ratio = (AspectRatio + 15) / 64 }
  end;

  TGIFHeader = class(TGIFItem)
  private
    FLogicalScreenDescriptor: TLogicalScreenDescriptor;
    FColorMap		: TGIFColorMap;
    procedure Prepare;
  protected
    function GetVersion: TGIFVersion; override;
    function GetBackgroundColor: TColor;
    procedure SetBackgroundColor(Color: TColor);
    procedure SetBackgroundColorIndex(Index: BYTE);
    function GetBitsPerPixel: integer;
    function GetColorResolution: integer;
  public
    constructor Create(GIFImage: TGIFImage); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure Clear;
    property Version: TGIFVersion read GetVersion;
    property Width: WORD read FLogicalScreenDescriptor.ScreenWidth
                         write FLogicalScreenDescriptor.ScreenWidth;
    property Height: WORD read FLogicalScreenDescriptor.ScreenHeight
                          write FLogicalScreenDescriptor.Screenheight;
    property BackgroundColorIndex: BYTE read FLogicalScreenDescriptor.BackgroundColorIndex
                                        write SetBackgroundColorIndex;
    property BackgroundColor: TColor read GetBackgroundColor
                                     write SetBackgroundColor;
    property AspectRatio: BYTE read FLogicalScreenDescriptor.AspectRatio
                               write FLogicalScreenDescriptor.AspectRatio;
    property ColorMap: TGIFColorMap read FColorMap;
    property BitsPerPixel: integer read GetBitsPerPixel;
    property ColorResolution: integer read GetColorResolution;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFExtension
//
////////////////////////////////////////////////////////////////////////////////
  TGIFExtensionType = BYTE;
  TGIFExtension = class;
  TGIFExtensionClass = class of TGIFExtension;

  TGIFGraphicControlExtension = class;

  TGIFExtension = class(TGIFItem)
  private
    FSubImage: TGIFSubImage;
  protected
    function GetExtensionType: TGIFExtensionType; virtual; abstract;
    function GetVersion: TGIFVersion; override;
    function DoReadFromStream(Stream: TStream): TGIFExtensionType;
    class procedure RegisterExtension(elabel: BYTE; eClass: TGIFExtensionClass);
    class function FindExtension(Stream: TStream): TGIFExtensionClass;
    class function FindSubExtension(Stream: TStream): TGIFExtensionClass; virtual;
  public
     // Ignore compiler warning about hiding base class constructor
    constructor Create(ASubImage: TGIFSubImage); {$IFDEF VER12_PLUS} reintroduce; {$ENDIF} virtual;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    property ExtensionType: TGIFExtensionType read GetExtensionType;
    property SubImage: TGIFSubImage read FSubImage;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFSubImage
//
////////////////////////////////////////////////////////////////////////////////
  TGIFExtensionList = class(TGIFList)
  protected
    function GetExtension(Index: Integer): TGIFExtension;
    procedure SetExtension(Index: Integer; Extension: TGIFExtension);
  public
    procedure LoadFromStream(Stream: TStream; Parent: TObject); override;
    property Extensions[Index: Integer]: TGIFExtension read GetExtension write SetExtension; default;
  end;

  TImageDescriptor = packed record
    Separator: byte;	{ fixed value of ImageSeparator }
    Left: word;		{ Column in pixels in respect to left edge of logical screen }
    Top: word;		{ row in pixels in respect to top of logical screen }
    Width: word;	{ width of image in pixels }
    Height: word;	{ height of image in pixels }
    PackedFields: byte;	{ Bit fields }
  end;

  TGIFSubImage = class(TGIFItem)
  private
    FBitmap		: TBitmap;
    FMask		: HBitmap;
    FNeedMask		: boolean;
    FLocalPalette	: HPalette;
    FData		: PChar;
    FDataSize		: integer;
    FColorMap		: TGIFColorMap;
    FImageDescriptor	: TImageDescriptor;
    FExtensions		: TGIFExtensionList;
    FTransparent	: boolean;
    FGCE		: TGIFGraphicControlExtension;
    procedure Prepare;
    procedure Compress(Stream: TStream);
    procedure Decompress(Stream: TStream);
  protected
    function GetVersion: TGIFVersion; override;
    function GetInterlaced: boolean;
    procedure SetInterlaced(Value: boolean);
    function GetColorResolution: integer;
    function GetBitsPerPixel: integer;
    procedure AssignTo(Dest: TPersistent); override;
    function DoGetBitmap: TBitmap;
    function DoGetDitherBitmap: TBitmap;
    function GetBitmap: TBitmap;
    procedure SetBitmap(Value: TBitmap);
    procedure FreeMask;
    function GetEmpty: Boolean;
    function GetPalette: HPALETTE;
    procedure SetPalette(Value: HPalette);
    function GetActiveColorMap: TGIFColorMap;
    function GetBoundsRect: TRect;
    procedure SetBoundsRect(const Value: TRect);
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
    function GetClientRect: TRect;
    function GetPixel(x, y: integer): BYTE;
    function GetScanline(y: integer): pointer;
    procedure NewBitmap;
    procedure FreeBitmap;
    procedure NewImage;
    procedure FreeImage;
    procedure NeedImage;
    function ScaleRect(DestRect: TRect): TRect;
    function HasMask: boolean;
    function GetBounds(Index: integer): WORD;
    procedure SetBounds(Index: integer; Value: WORD);
    function GetHasBitmap: boolean;
    procedure SetHasBitmap(Value: boolean);
  public
    constructor Create(GIFImage: TGIFImage); override;
    destructor Destroy; override;
    procedure Clear;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect;
      DoTransparent, DoTile: boolean);
    procedure StretchDraw(ACanvas: TCanvas; const Rect: TRect;
      DoTransparent, DoTile: boolean);
    procedure Crop;
    procedure Merge(Previous: TGIFSubImage);
    property HasBitmap: boolean read GetHasBitmap write SetHasBitmap;
    property Left: WORD index 1 read GetBounds write SetBounds;
    property Top: WORD index 2 read GetBounds write SetBounds;
    property Width: WORD index 3 read GetBounds write SetBounds;
    property Height: WORD index 4 read GetBounds write SetBounds;
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property ClientRect: TRect read GetClientRect;
    property Interlaced: boolean read GetInterlaced write SetInterlaced;
    property ColorMap: TGIFColorMap read FColorMap;
    property ActiveColorMap: TGIFColorMap read GetActiveColorMap;
    property Data: PChar read FData;
    property DataSize: integer read FDataSize;
    property Extensions: TGIFExtensionList read FExtensions;
    property Version: TGIFVersion read GetVersion;
    property ColorResolution: integer read GetColorResolution;
    property BitsPerPixel: integer read GetBitsPerPixel;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Mask: HBitmap read FMask;
    property Palette: HPALETTE read GetPalette write SetPalette;
    property Empty: boolean read GetEmpty;
    property Transparent: boolean read FTransparent;
    property GraphicControlExtension: TGIFGraphicControlExtension read FGCE;
    property Pixels[x, y: integer]: BYTE read GetPixel;
    property Scanline[y: integer]: pointer read GetScanline;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFTrailer
//
////////////////////////////////////////////////////////////////////////////////
  TGIFTrailer = class(TGIFItem)
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFGraphicControlExtension
//
////////////////////////////////////////////////////////////////////////////////
  // Graphic Control Extension block a.k.a GCE
  TGIFGCERec = packed record
    BlockSize: byte;         { should be 4 }
    PackedFields: Byte;
    DelayTime: Word;         { in centiseconds }
    TransparentColorIndex: Byte;
    Terminator: Byte;
  end;

  TDisposalMethod = (dmNone, dmNoDisposal, dmBackground, dmPrevious);

  TGIFGraphicControlExtension = class(TGIFExtension)
  private
    FGCExtension: TGIFGCERec;
  protected
    function GetExtensionType: TGIFExtensionType; override;
    function GetTransparent: boolean;
    procedure SetTransparent(Value: boolean);
    function GetTransparentColor: TColor;
    procedure SetTransparentColor(Color: TColor);
    function GetTransparentColorIndex: BYTE;
    procedure SetTransparentColorIndex(Value: BYTE);
    function GetDelay: WORD;
    procedure SetDelay(Value: WORD);
    function GetUserInput: boolean;
    procedure SetUserInput(Value: boolean);
    function GetDisposal: TDisposalMethod;
    procedure SetDisposal(Value: TDisposalMethod);

  public
    constructor Create(ASubImage: TGIFSubImage); override;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    property Delay: WORD read GetDelay write SetDelay;
    property Transparent: boolean read GetTransparent write SetTransparent;
    property TransparentColorIndex: BYTE read GetTransparentColorIndex
                                            write SetTransparentColorIndex;
    property TransparentColor: TColor read GetTransparentColor write SetTransparentColor;
    property UserInput: boolean read GetUserInput write SetUserInput;
    property Disposal: TDisposalMethod read GetDisposal write SetDisposal;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFTextExtension
//
////////////////////////////////////////////////////////////////////////////////
  TGIFPlainTextExtensionRec = packed record
    BlockSize: byte;         { should be 12 }
    Left, Top, Width, Height: Word;
    CellWidth, CellHeight: Byte;
    TextFGColorIndex,
    TextBGColorIndex: Byte;
  end;

  TGIFTextExtension = class(TGIFExtension)
  private
    FText		: TStrings;
    FPlainTextExtension	: TGIFPlainTextExtensionRec;
  protected
    function GetExtensionType: TGIFExtensionType; override;
    function GetForegroundColor: TColor;
    procedure SetForegroundColor(Color: TColor);
    function GetBackgroundColor: TColor;
    procedure SetBackgroundColor(Color: TColor);
    function GetBounds(Index: integer): WORD;
    procedure SetBounds(Index: integer; Value: WORD);
    function GetCharWidthHeight(Index: integer): BYTE;
    procedure SetCharWidthHeight(Index: integer; Value: BYTE);
    function GetColorIndex(Index: integer): BYTE;
    procedure SetColorIndex(Index: integer; Value: BYTE);
  public
    constructor Create(ASubImage: TGIFSubImage); override;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    property Left: WORD index 1 read GetBounds write SetBounds;
    property Top: WORD index 2 read GetBounds write SetBounds;
    property GridWidth: WORD index 3 read GetBounds write SetBounds;
    property GridHeight: WORD index 4 read GetBounds write SetBounds;
    property CharWidth: BYTE index 1 read GetCharWidthHeight write SetCharWidthHeight;
    property CharHeight: BYTE index 2 read GetCharWidthHeight write SetCharWidthHeight;
    property ForegroundColorIndex: BYTE index 1 read GetColorIndex write SetColorIndex;
    property ForegroundColor: TColor read GetForegroundColor;
    property BackgroundColorIndex: BYTE  index 2 read GetColorIndex write SetColorIndex;
    property BackgroundColor: TColor read GetBackgroundColor;
    property Text: TStrings read FText write FText;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFCommentExtension
//
////////////////////////////////////////////////////////////////////////////////
  TGIFCommentExtension = class(TGIFExtension)
  private
    FText		: TStrings;
  protected
    function GetExtensionType: TGIFExtensionType; override;
  public
    constructor Create(ASubImage: TGIFSubImage); override;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    property Text: TStrings read FText;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFApplicationExtension
//
////////////////////////////////////////////////////////////////////////////////
  TGIFIdentifierCode = array[0..7] of char;
  TGIFAuthenticationCode = array[0..2] of char;
  TGIFApplicationRec = packed record
    Identifier		: TGIFIdentifierCode;
    Authentication	: TGIFAuthenticationCode;
  end;

  TGIFApplicationExtension = class;
  TGIFAppExtensionClass = class of TGIFApplicationExtension;

  TGIFApplicationExtension = class(TGIFExtension)
  private
    FIdent		: TGIFApplicationRec;
    function GetAuthentication: string;
    function GetIdentifier: string;
  protected
    function GetExtensionType: TGIFExtensionType; override;
    procedure SetAuthentication(const Value: string);
    procedure SetIdentifier(const Value: string);
    procedure SaveData(Stream: TStream); virtual; abstract;
    procedure LoadData(Stream: TStream); virtual; abstract;
  public
    constructor Create(ASubImage: TGIFSubImage); override;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    class procedure RegisterExtension(eIdent: TGIFApplicationRec; eClass: TGIFAppExtensionClass);
    class function FindSubExtension(Stream: TStream): TGIFExtensionClass; override;
    property Identifier: string read GetIdentifier write SetIdentifier;
    property Authentication: string read GetAuthentication write SetAuthentication;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFUnknownAppExtension
//
////////////////////////////////////////////////////////////////////////////////
  TGIFBlock = class(TObject)
  private
    FSize		: BYTE;
    FData		: pointer;
  public
    constructor Create(ASize: integer);
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    property Size: BYTE read FSize;
    property Data: pointer read FData;
  end;

  TGIFUnknownAppExtension = class(TGIFApplicationExtension)
  private
    FBlocks		: TList;
  protected
    procedure SaveData(Stream: TStream); override;
    procedure LoadData(Stream: TStream); override;
  public
    constructor Create(ASubImage: TGIFSubImage); override;
    destructor Destroy; override;
    property Blocks: TList read FBlocks;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFAppExtNSLoop
//
////////////////////////////////////////////////////////////////////////////////
  TGIFAppExtNSLoop = class(TGIFApplicationExtension)
  private
    FLoops		: WORD;
    FBufferSize		: DWORD;
  protected
    procedure SaveData(Stream: TStream); override;
    procedure LoadData(Stream: TStream); override;
  public
    constructor Create(ASubImage: TGIFSubImage); override;
    property Loops: WORD read FLoops write FLoops;
    property BufferSize: DWORD read FBufferSize write FBufferSize;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFImage
//
////////////////////////////////////////////////////////////////////////////////
  TGIFImageList = class(TGIFList)
  protected
    function GetImage(Index: Integer): TGIFSubImage;
    procedure SetImage(Index: Integer; SubImage: TGIFSubImage);
  public
    procedure LoadFromStream(Stream: TStream; Parent: TObject); override;
    procedure SaveToStream(Stream: TStream); override;
    property SubImages[Index: Integer]: TGIFSubImage read GetImage write SetImage; default;
  end;

  // Compression algorithms
  TGIFCompression =
    (gcLZW,			// Normal LZW compression
     gcRLE			// GIF compatible RLE compression
    );

  // Color reduction methods
  TColorReduction =
    (rmNone,			// Do not perform color reduction
     rmWindows20,		// Reduce to the Windows 20 color system palette
     rmWindows256,		// Reduce to the Windows 256 color halftone palette (Only works in 256 color display mode)
     rmWindowsGray,		// Reduce to the Windows 4 grayscale colors
     rmMonochrome,		// Reduce to a black/white monochrome palette
     rmGrayScale,		// Reduce to a uniform 256 shade grayscale palette
     rmNetscape,		// Reduce to the Netscape 216 color palette
     rmQuantize,		// Reduce to optimal 2^n color palette
     rmQuantizeWindows,		// Reduce to optimal 256 color windows palette
     rmPalette			// Reduce to custom palette
    );
  TDitherMode =
    (dmNearest,			// Nearest color matching w/o error correction
     dmFloydSteinberg,		// Floyd Steinberg Error Diffusion dithering
     dmStucki,			// Stucki Error Diffusion dithering
     dmSierra,			// Sierra Error Diffusion dithering
     dmJaJuNI,			// Jarvis, Judice & Ninke Error Diffusion dithering
     dmSteveArche,		// Stevenson & Arche Error Diffusion dithering
     dmBurkes			// Burkes Error Diffusion dithering
     // dmOrdered,		// Ordered dither
    );

  // Optimization options
  TGIFOptimizeOption =
    (ooCrop,			// Crop animated GIF frames
     ooMerge,			// Merge pixels of same color
     ooCleanup,			// Remove comments and application extensions
     ooColorMap,		// Sort color map by usage and remove unused entries
     ooReduceColors		// Reduce color depth ***NOT IMPLEMENTED***
    );
  TGIFOptimizeOptions = set of TGIFOptimizeOption;

  TGIFDrawOption =
    (goAsync,			// Asyncronous draws (paint in thread)
     goTransparent,		// Transparent draws
     goAnimate,			// Animate draws
     goLoop,			// Loop animations
     goLoopContinously,		// Ignore loop count and loop forever
     goValidateCanvas,		// Validate canvas in threaded paint ***NOT IMPLEMENTED***
     goDirectDraw,		// Draw() directly on canvas
     goClearOnLoop,		// Clear animation on loop
     goTile,			// Tiled display
     goDither,			// Dither to Netscape palette
     goAutoDither		// Only dither on 256 color systems
    );
  TGIFDrawOptions = set of TGIFDrawOption;
  // Note: if goAsync is not set then goDirectDraw should be set. Otherwise
  // the image will not be displayed.

  PGIFPainter = ^TGIFPainter;

  TGIFPainter = class(TThread)
  private
    FImage		: TGIFImage;	// The TGIFImage that owns this painter
    FCanvas		: TCanvas;	// Destination canvas
    FRect		: TRect;	// Destination rect
    FDrawOptions	: TGIFDrawOptions;// Paint options
    FAnimationSpeed	: integer;	// Animation speed %
    FActiveImage	: integer;	// Current frame
    Disposal		,		// Used by synchronized paint
    OldDisposal		: TDisposalMethod;// Used by synchronized paint
    BackupBuffer	: TBitmap;	// Used by synchronized paint
    FrameBuffer		: TBitmap;	// Used by synchronized paint
    Background		: TBitmap;	// Used by synchronized paint
    ValidateDC		: HDC;
    DoRestart		: boolean;	// Flag used to restart animation
    FStarted		: boolean;	// Flag used to signal start of paint
    PainterRef		: PGIFPainter;	// Pointer to var referencing painter
    FEventHandle	: THandle;	// Animation delay event
    ExceptObject	: Exception;	// Eaten exception
    ExceptAddress	: pointer;	// Eaten exceptions address
    FEvent		: TNotifyEvent;	// Used by synchronized events
    FOnStartPaint	: TNotifyEvent;
    FOnPaint		: TNotifyEvent;
    FOnAfterPaint	: TNotifyEvent;
    FOnLoop		: TNotifyEvent;
    FOnEndPaint		: TNotifyEvent;
    procedure DoOnTerminate(Sender: TObject);// Sync. shutdown procedure
    procedure DoSynchronize(Method: TThreadMethod);// Conditional sync stub
{$ifdef SERIALIZE_RENDER}
    procedure PrefetchBitmap;		// Sync. bitmap prefetch
{$endif}
    procedure DoPaintFrame;		// Sync. buffered paint procedure
    procedure DoPaint;			// Sync. paint procedure
    procedure DoEvent;
    procedure SetActiveImage(const Value: integer);// Sync. event procedure
  protected
    procedure Execute; override;
    procedure SetAnimationSpeed(Value: integer);
  public
    constructor Create(AImage: TGIFImage; ACanvas: TCanvas; ARect: TRect;
      Options: TGIFDrawOptions);
    constructor CreateRef(Painter: PGIFPainter; AImage: TGIFImage; ACanvas: TCanvas; ARect: TRect;
      Options: TGIFDrawOptions);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Restart;
    property Image: TGIFImage read FImage;
    property Canvas: TCanvas read FCanvas;
    property Rect: TRect read FRect write FRect;
    property DrawOptions: TGIFDrawOptions read FDrawOptions write FDrawOptions;
    property AnimationSpeed: integer read FAnimationSpeed write SetAnimationSpeed;
    property Started: boolean read FStarted;
    property ActiveImage: integer read FActiveImage write SetActiveImage;
    property OnStartPaint: TNotifyEvent read FOnStartPaint write FOnStartPaint;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnAfterPaint: TNotifyEvent read FOnAfterPaint write FOnAfterPaint;
    property OnLoop: TNotifyEvent read FOnLoop write FOnLoop;
    property OnEndPaint	: TNotifyEvent read FOnEndPaint	 write FOnEndPaint	;
    property EventHandle: THandle read FEventHandle;
  end;

  TGIFWarning = procedure(Sender: TObject; Severity: TGIFSeverity; Message: string) of object;

  TGIFImage = class(TGraphic)
  private
    IsDrawing		: Boolean;
    IsInsideGetPalette	: boolean;
    FImages		: TGIFImageList;
    FHeader		: TGIFHeader;
    FGlobalPalette	: HPalette;
    FPainters		: TThreadList;
    FDrawOptions	: TGIFDrawOptions;
    FColorReduction	: TColorReduction;
    FReductionBits	: integer;
    FDitherMode		: TDitherMode;
    FCompression	: TGIFCompression;
    FOnWarning		: TGIFWarning;
    FBitmap		: TBitmap;
    FDrawPainter	: TGIFPainter;
    FThreadPriority	: TThreadPriority;
    FAnimationSpeed	: integer;
    FDrawBackgroundColor: TColor;
    FOnStartPaint	: TNotifyEvent;
    FOnPaint		: TNotifyEvent;
    FOnAfterPaint	: TNotifyEvent;
    FOnLoop		: TNotifyEvent;
    FOnEndPaint		: TNotifyEvent;
{$IFDEF VER9x}
    FPaletteModified	: Boolean;
    FOnProgress		: TProgressEvent;
{$ENDIF}
  protected
    // Obsolete: procedure Changed(Sender: TObject); {$IFDEF VER9x} virtual; {$ELSE} override; {$ENDIF}
    function GetHeight: Integer; override;
    procedure SetHeight(Value: Integer); override;
    function GetWidth: Integer; override;
    procedure SetWidth(Value: Integer); override;
    procedure AssignTo(Dest: TPersistent); override;
    function InternalPaint(Painter: PGIFPainter; ACanvas: TCanvas; const Rect: TRect; Options: TGIFDrawOptions): TGIFPainter;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function Equals(Graphic: TGraphic): Boolean; override;
    function GetPalette: HPALETTE; {$IFDEF VER9x} virtual; {$ELSE} override; {$ENDIF}
    procedure SetPalette(Value: HPalette); {$IFDEF VER9x} virtual; {$ELSE} override; {$ENDIF}
    function GetEmpty: Boolean; override;
    procedure WriteData(Stream: TStream); override;
    function GetIsTransparent: Boolean;
    function GetVersion: TGIFVersion;
    function GetColorResolution: integer;
    function GetBitsPerPixel: integer;
    function GetBackgroundColorIndex: BYTE;
    procedure SetBackgroundColorIndex(const Value: BYTE);
    function GetBackgroundColor: TColor;
    procedure SetBackgroundColor(const Value: TColor);
    function GetAspectRatio: BYTE;
    procedure SetAspectRatio(const Value: BYTE);
    procedure SetDrawOptions(Value: TGIFDrawOptions);
    procedure SetAnimationSpeed(Value: integer);
    procedure SetReductionBits(Value: integer);
    procedure NewImage;
    function GetBitmap: TBitmap;
    function NewBitmap: TBitmap;
    procedure FreeBitmap;
    function GetColorMap: TGIFColorMap;
    function GetDoDither: boolean;
    property DrawPainter: TGIFPainter read FDrawPainter; // Extremely volatile
    property DoDither: boolean read GetDoDither;
{$IFDEF VER9x}
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
{$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    function Add(Source: TPersistent): integer;
    procedure Pack;
    procedure OptimizeColorMap;
    procedure Optimize(Options: TGIFOptimizeOptions;
      ColorReduction: TColorReduction; DitherMode: TDitherMode;
      ReductionBits: integer);
    procedure Clear;
    procedure StopDraw;
    function Paint(ACanvas: TCanvas; const Rect: TRect; Options: TGIFDrawOptions): TGIFPainter;
    procedure PaintStart;
    procedure PaintPause;
    procedure PaintStop;
    procedure PaintResume;
    procedure PaintRestart;
    procedure Warning(Sender: TObject; Severity: TGIFSeverity; Message: string); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    property GlobalColorMap: TGIFColorMap read GetColorMap;
    property Version: TGIFVersion read GetVersion;
    property Images: TGIFImageList read FImages;
    property ColorResolution: integer read GetColorResolution;
    property BitsPerPixel: integer read GetBitsPerPixel;
    property BackgroundColorIndex: BYTE read GetBackgroundColorIndex write SetBackgroundColorIndex;
    property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor;
    property AspectRatio: BYTE read GetAspectRatio write SetAspectRatio;
    property Header: TGIFHeader read FHeader; // ***OBSOLETE***
    property IsTransparent: boolean read GetIsTransparent;
    property DrawOptions: TGIFDrawOptions read FDrawOptions write SetDrawOptions;
    property DrawBackgroundColor: TColor read FDrawBackgroundColor write FDrawBackgroundColor;
    property ColorReduction: TColorReduction read FColorReduction write FColorReduction;
    property ReductionBits: integer read FReductionBits write SetReductionBits;
    property DitherMode: TDitherMode read FDitherMode write FDitherMode;
    property Compression: TGIFCompression read FCompression write FCompression;
    property AnimationSpeed: integer read FAnimationSpeed write SetAnimationSpeed;
    property Painters: TThreadList read FPainters;
    property ThreadPriority: TThreadPriority read FThreadPriority write FThreadPriority;
    property Bitmap: TBitmap read GetBitmap; // Volatile - beware!
    property OnWarning: TGIFWarning read FOnWarning write FOnWarning;
    property OnStartPaint: TNotifyEvent read FOnStartPaint write FOnStartPaint;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnAfterPaint: TNotifyEvent read FOnAfterPaint write FOnAfterPaint;
    property OnLoop: TNotifyEvent read FOnLoop write FOnLoop;
    property OnEndPaint	: TNotifyEvent read FOnEndPaint	 write FOnEndPaint	;
{$IFDEF VER9x}
    property Palette: HPALETTE read GetPalette write SetPalette;
    property PaletteModified: Boolean read FPaletteModified write FPaletteModified;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
{$ENDIF}
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      Utility routines
//
////////////////////////////////////////////////////////////////////////////////
  // WebPalette creates a 216 color uniform palette a.k.a. the Netscape Palette
  function WebPalette: HPalette;

  // ReduceColors
  // Map colors in a bitmap to their nearest representation in a palette using
  // the methods specified by the ColorReduction and DitherMode parameters.
  // The ReductionBits parameter specifies the desired number of colors (bits
  // per pixel) when the reduction method is rmQuantize. The CustomPalette
  // specifies the palette when the rmPalette reduction method is used.
  function ReduceColors(Bitmap: TBitmap; ColorReduction: TColorReduction;
    DitherMode: TDitherMode; ReductionBits: integer; CustomPalette: hPalette): TBitmap;

  // CreateOptimizedPaletteFromManyBitmaps
  //: Performs Color Quantization on multiple bitmaps.
  // The Bitmaps parameter is a list of bitmaps. Returns an optimized palette.
  function CreateOptimizedPaletteFromManyBitmaps(Bitmaps: TList; Colors, ColorBits: integer;
    Windows: boolean): hPalette;

{$IFDEF VER9x}
  // From Delphi 3 graphics.pas
type
  TPixelFormat = (pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit, pf32bit, pfCustom);
{$ENDIF}

  procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: Integer;
    var ImageSize: longInt; PixelFormat: TPixelFormat);
  function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
   var BitmapInfo; var Bits; PixelFormat: TPixelFormat): Boolean;

////////////////////////////////////////////////////////////////////////////////
//
//                      Global variables
//
////////////////////////////////////////////////////////////////////////////////
// GIF Clipboard format identifier for use by LoadFromClipboardFormat and
// SaveToClipboardFormat.
// Set in Initialization section.
var
  CF_GIF: WORD;

////////////////////////////////////////////////////////////////////////////////
//
//                      Library defaults
//
////////////////////////////////////////////////////////////////////////////////
var
  //: Default options for TGIFImage.DrawOptions.
  GIFImageDefaultDrawOptions : TGIFDrawOptions =
    [goAsync, goLoop, goTransparent, goAnimate, goDither, goAutoDither
{$IFDEF STRICT_MOZILLA}
     ,goClearOnLoop
{$ENDIF}
    ];

  // WARNING! Do not use goAsync and goDirectDraw unless you have absolute
  // control of the destination canvas.
  // TGIFPainter will continue to write on the canvas even after the canvas has
  // been deleted, unless *you* prevent it.
  // The goValidateCanvas option will fix this problem if it is ever implemented.

  //: Default color reduction methods for bitmap import.
  // These are the fastest settings, but also the ones that gives the
  // worst result (in most cases).
  GIFImageDefaultColorReduction: TColorReduction = rmNetscape;
  GIFImageDefaultColorReductionBits: integer = 8; // Range 3 - 8
  GIFImageDefaultDitherMode: TDitherMode = dmNearest;

  //: Default encoder compression method.
  GIFImageDefaultCompression: TGIFCompression = gcLZW;

  //: Default painter thread priority
  GIFImageDefaultThreadPriority: TThreadPriority = tpNormal;

  //: Default animation speed in % of normal speed (range 0 - 1000)
  GIFImageDefaultAnimationSpeed: integer = 100;

  // DoAutoDither is set to True in the initializaion section if the desktop DC
  // supports 256 colors or less.
  // It can be modified in your application to disable/enable Auto Dithering
  DoAutoDither: boolean = False;

  // Palette is set to True in the initialization section if the desktop DC
  // supports 256 colors or less.
  // You should NOT modify it.
  PaletteDevice: boolean = False;

  // Set GIFImageRenderOnLoad to True to render (convert to bitmap) the
  // GIF frames as they are loaded instead of rendering them on-demand.
  // This might increase resource consumption and will increase load time,
  // but will cause animated GIFs to display more smoothly.
  GIFImageRenderOnLoad: boolean = False;

  // If GIFImageOptimizeOnStream is true, the GIF will be optimized
  // before it is streamed to the DFM file.
  // This will not affect TGIFImage.SaveToStream or SaveToFile.
  GIFImageOptimizeOnStream: boolean = False;

////////////////////////////////////////////////////////////////////////////////
//
//                      Design Time support
//
////////////////////////////////////////////////////////////////////////////////
// Dummy component registration for design time support of GIFs in TImage
procedure Register;

////////////////////////////////////////////////////////////////////////////////
//
//                      Error messages
//
////////////////////////////////////////////////////////////////////////////////
{$ifndef VER9x}
resourcestring
{$else}
const
{$endif}
  // GIF Error messages
  sOutOfData		= 'Premature end of data';
  sTooManyColors	= 'Color table overflow';
  sBadColorIndex	= 'Invalid color index';
  sBadVersion		= 'Unsupported GIF version';
  sBadSignature		= 'Invalid GIF signature';
  sScreenBadColorSize	= 'Invalid number of colors specified in Screen Descriptor';
  sImageBadColorSize	= 'Invalid number of colors specified in Image Descriptor';
  sUnknownExtension	= 'Unknown extension type';
  sBadExtensionLabel	= 'Invalid extension introducer';
  sOutOfMemDIB		= 'Failed to allocate memory for GIF DIB';
  sDIBCreate		= 'Failed to create DIB from Bitmap';
  sDecodeTooFewBits	= 'Decoder bit buffer under-run';
  sDecodeCircular	= 'Circular decoder table entry';
  sBadTrailer		= 'Invalid Image trailer';
  sBadExtensionInstance	= 'Internal error: Extension Instance does not match Extension Label';
  sBadBlockSize		= 'Unsupported Application Extension block size';
  sBadBlock		= 'Unknown GIF block type';
  sUnsupportedClass	= 'Object type not supported for operation';
  sInvalidData		= 'Invalid GIF data';
  sBadHeight		= 'Image height too small for contained frames';
  sBadWidth		= 'Image width too small for contained frames';
{$IFNDEF REGISTER_TGIFIMAGE}
  sGIFToClipboard	= 'Clipboard operations not supported for GIF objects';
{$ELSE}
  sFailedPaste		= 'Failed to store GIF on clipboard';
{$IFDEF VER9x}
  sUnknownClipboardFormat= 'Unsupported clipboard format';
{$ENDIF}
{$ENDIF}
  sScreenSizeExceeded	= 'Image exceeds Logical Screen size';
  sNoColorTable		= 'No global or local color table defined';
  sBadPixelCoordinates	= 'Invalid pixel coordinates';
  sUnsupportedBitmap	= 'Unsupported bitmap format';
  sInvalidPixelFormat	= 'Unsupported PixelFormat';
  sBadDimension		= 'Invalid image dimensions';
  sNoDIB		= 'Image has no DIB';
  sInvalidStream	= 'Invalid stream operation';
  sInvalidColor		= 'Color not in color table';
  sInvalidBitSize	= 'Invalid Bits Per Pixel value';
  sEmptyColorMap	= 'Color table is empty';
  sEmptyImage		= 'Image is empty';
  sInvalidBitmapList	= 'Invalid bitmap list';
  sInvalidReduction	= 'Invalid reduction method';
{$IFDEF VER9x}
  // From Delphi 3 consts.pas
  SOutOfResources	= 'Out of system resources';
  SInvalidBitmap	= 'Bitmap image is not valid';
  SScanLine		= 'Scan line index out of range';
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
//                      Misc texts
//
////////////////////////////////////////////////////////////////////////////////
  // File filter name
  sGIFImageFile		= 'GIF Image';

  // Progress messages
  sProgressLoading	= 'Loading...';
  sProgressSaving	= 'Saving...';
  sProgressConverting	= 'Converting...';
  sProgressRendering	= 'Rendering...';
  sProgressCopying	= 'Copying...';
  sProgressOptimizing	= 'Optimizing...';


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
//			Implementation
//
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
implementation

{ This makes me long for the C preprocessor... }
{$ifdef DEBUG}
  {$ifdef DEBUG_COMPRESSPERFORMANCE}
    {$define DEBUG_PERFORMANCE}
  {$else}
    {$ifdef DEBUG_DECOMPRESSPERFORMANCE}
      {$define DEBUG_PERFORMANCE}
    {$else}
      {$ifdef DEBUG_DITHERPERFORMANCE}
        {$define DEBUG_PERFORMANCE}
      {$else}
        {$ifdef DEBUG_DITHERPERFORMANCE}
          {$define DEBUG_PERFORMANCE}
        {$else}
          {$ifdef DEBUG_DRAWPERFORMANCE}
            {$define DEBUG_PERFORMANCE}
          {$else}
            {$ifdef DEBUG_RENDERPERFORMANCE}
              {$define DEBUG_PERFORMANCE}
            {$endif}
          {$endif}
        {$endif}
      {$endif}
    {$endif}
  {$endif}
{$endif}

uses
{$ifdef DEBUG}
  dialogs,
{$endif}
  mmsystem, // timeGetTime()
  messages,
  Consts;


////////////////////////////////////////////////////////////////////////////////
//
//			Misc consts
//
////////////////////////////////////////////////////////////////////////////////
const
  { Extension/block label values }
  bsPlainTextExtension		= $01;
  bsGraphicControlExtension	= $F9;
  bsCommentExtension		= $FE;
  bsApplicationExtension	= $FF;

  bsImageDescriptor		= Ord(',');
  bsExtensionIntroducer		= Ord('!');
  bsTrailer			= ord(';');

  // Thread messages - Used by TThread.Synchronize()
  CM_DESTROYWINDOW	= $8FFE; // Defined in classes.pas
  CM_EXECPROC 		= $8FFF; // Defined in classes.pas


////////////////////////////////////////////////////////////////////////////////
//
//                      Design Time support
//
////////////////////////////////////////////////////////////////////////////////
//: Dummy component registration to add design-time support of GIFs to TImage.
// Since TGIFImage isn't a component there's nothing to register here, but
// since Register is only called at design time we can set the design time
// GIF paint options here (modify as you please):
procedure Register;
begin
  // Don't loop animations at design-time. Animated GIFs will animate once and
  // then stop thus not using CPU resources and distracting the developer.
  Exclude(GIFImageDefaultDrawOptions, goLoop);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Utilities
//
////////////////////////////////////////////////////////////////////////////////
//: Creates a 216 color uniform non-dithering Netscape palette.
function WebPalette: HPalette;
type
  TLogWebPalette	= packed record
    palVersion		: word;
    palNumEntries	: word;
    PalEntries		: array[0..5,0..5,0..5] of TPaletteEntry;
  end;
var
  r, g, b		: byte;
  LogWebPalette		: TLogWebPalette;
  LogPalette		: TLogpalette absolute LogWebPalette; // Stupid typecast
begin
  with LogWebPalette do
  begin
    palVersion:= $0300;
    palNumEntries:= 216;
    for r:=0 to 5 do
      for g:=0 to 5 do
        for b:=0 to 5 do
        begin
          with PalEntries[r,g,b] do
          begin
            peRed := 51 * r;
            peGreen := 51 * g;
            peBlue := 51 * b;
            peFlags := 0;
          end;
        end;
  end;
  Result := CreatePalette(Logpalette);
end;

(*
**  GDI Error handling
**  Adapted from graphics.pas
*)
{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
{$ifdef D3_BCB3}
function GDICheck(Value: Integer): Integer;
{$else}
function GDICheck(Value: Cardinal): Cardinal;
{$endif}
var
  ErrorCode		: integer;
  Buf			: array [byte] of char;

  function ReturnAddr: Pointer;
  // From classes.pas
  asm
    MOV		EAX,[EBP+4] // sysutils.pas says [EBP-4], but this works !
  end;

begin
  if (Value = 0) then
  begin
    ErrorCode := GetLastError;
    if (ErrorCode <> 0) and (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
      ErrorCode, LOCALE_USER_DEFAULT, Buf, sizeof(Buf), nil) <> 0) then
      raise EOutOfResources.Create(Buf) at ReturnAddr
    else
      raise EOutOfResources.Create(SOutOfResources) at ReturnAddr;
  end;
  Result := Value;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

(*
**  Raise error condition
*)
procedure Error(msg: string);
  function ReturnAddr: Pointer;
  // From classes.pas
  asm
    MOV		EAX,[EBP+4] // sysutils.pas says [EBP-4] !
  end;
begin
  raise GIFException.Create(msg) at ReturnAddr;
end;

(*
**  Return number bytes required to
**  hold a given number of bits.
*)
function ByteAlignBit(Bits: Cardinal): Cardinal;
begin
  Result := (Bits+7) SHR 3;
end;
// Rounded up to nearest 2
function WordAlignBit(Bits: Cardinal): Cardinal;
begin
  Result := ((Bits+15) SHR 4) SHL 1;
end;
// Rounded up to nearest 4
function DWordAlignBit(Bits: Cardinal): Cardinal;
begin
  Result := ((Bits+31) SHR 5) SHL 2;
end;
// Round to arbitrary number of bits
function AlignBit(Bits, BitsPerPixel, Alignment: Cardinal): Cardinal;
begin
  Dec(Alignment);
  Result := ((Bits * BitsPerPixel) + Alignment) and not Alignment;
  Result := Result SHR 3;
end;

(*
**  Compute Bits per Pixel from Number of Colors
**  (Return the ceiling log of n)
*)
function Colors2bpp(Colors: integer): integer;
var
  MaxColor		: integer;
begin
  (*
  ** This might be faster computed by multiple if then else statements
  *)

  if (Colors = 0) then
    Result := 0
  else
  begin
    Result := 1;
    MaxColor := 2;
    while (Colors > MaxColor) do
    begin
      inc(Result);
      MaxColor := MaxColor SHL 1;
    end;
  end;
end;

(*
**  Write an ordinal byte value to a stream
*)
procedure WriteByte(Stream: TStream; b: BYTE);
begin
  Stream.Write(b, 1);
end;

(*
**  Read an ordinal byte value from a stream
*)
function ReadByte(Stream: TStream): BYTE;
begin
  Stream.Read(Result, 1);
end;

(*
**  Read data from stream and raise exception of EOF
*)
procedure ReadCheck(Stream: TStream; var Buffer; Size: LongInt);
var
  ReadSize		: integer;
begin
  ReadSize := Stream.Read(Buffer, Size);
  if (ReadSize <> Size) then
    Error(sOutOfData);
end;

(*
**  Write a string list to a stream as multiple blocks
**  of max 255 characters in each.
*)
procedure WriteStrings(Stream: TStream; Text: TStrings);
var
  i			: integer;
  b			: BYTE;
  size			: integer;
  s			: string;
begin
  for i := 0 to Text.Count-1 do
  begin
    s := Text[i];
    size := length(s);
    if (size > 255) then
      b := 255
    else
      b := size;
    while (size > 0) do
    begin
      dec(size, b);
      WriteByte(Stream, b);
      Stream.Write(PChar(s)^, b);
      delete(s, 1, b);
      if (b > size) then
        b := size;
    end;
  end;
  // Terminating zero (length = 0)
  WriteByte(Stream, 0);
end;


(*
**  Read a string list from a stream as multiple blocks
**  of max 255 characters in each.
*)
{ TODO -oanme -cImprovement : Replace ReadStrings with TGIFReader. }
procedure ReadStrings(Stream: TStream; Text: TStrings);
var
  size			: BYTE;
  buf			: array[0..255] of char;
begin
  Text.Clear;
  if (Stream.Read(size, 1) <> 1) then
    exit;
  while (size > 0) do
  begin
    ReadCheck(Stream, buf, size);
    buf[size] := #0;
    Text.Add(Buf);
    if (Stream.Read(size, 1) <> 1) then
      exit;
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//		Delphi 2.x / C++ Builder 1.x support
//
////////////////////////////////////////////////////////////////////////////////
{$IFDEF VER9x}
var
  // From Delphi 3 graphics.pas
  SystemPalette16: HPalette; // 16 color palette that maps to the system palette

type
  TPixelFormats = set of TPixelFormat;

const
  // Only pf1bit, pf4bit and pf8bit is supported since they are the only ones
  // with palettes
  SupportedPixelformats: TPixelFormats = [pf1bit, pf4bit, pf8bit];
{$ENDIF}


// --------------------------
// InitializeBitmapInfoHeader
// --------------------------
// Fills a TBitmapInfoHeader with the values of a bitmap when converted to a
// DIB of a specified PixelFormat.
//
// Parameters:
// Bitmap	The handle of the source bitmap.
// Info		The TBitmapInfoHeader buffer that will receive the values.
// PixelFormat	The pixel format of the destination DIB.
//
{$IFDEF BAD_STACK_ALIGNMENT}
  // Disable optimization to circumvent optimizer bug...
  {$IFOPT O+}
    {$DEFINE O_PLUS}
    {$O-}
  {$ENDIF}
{$ENDIF}
procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var Info: TBitmapInfoHeader;
  PixelFormat: TPixelFormat);
// From graphics.pas, "optimized" for our use
var
  DIB		: TDIBSection;
  Bytes		: Integer;
begin
  DIB.dsbmih.biSize := 0;
  Bytes := GetObject(Bitmap, SizeOf(DIB), @DIB);
  if (Bytes = 0) then
    Error(sInvalidBitmap);

  if (Bytes >= (sizeof(DIB.dsbm) + sizeof(DIB.dsbmih))) and
    (DIB.dsbmih.biSize >= sizeof(DIB.dsbmih)) then
    Info := DIB.dsbmih
  else
  begin
    FillChar(Info, sizeof(Info), 0);
    with Info, DIB.dsbm do
    begin
      biSize := SizeOf(Info);
      biWidth := bmWidth;
      biHeight := bmHeight;
    end;
  end;
  case PixelFormat of
    pf1bit: Info.biBitCount := 1;
    pf4bit: Info.biBitCount := 4;
    pf8bit: Info.biBitCount := 8;
    pf24bit: Info.biBitCount := 24;
  else
    Error(sInvalidPixelFormat);
    // Info.biBitCount := DIB.dsbm.bmBitsPixel * DIB.dsbm.bmPlanes;
  end;
  Info.biPlanes := 1;
  Info.biCompression := BI_RGB; // Always return data in RGB format
  Info.biSizeImage := AlignBit(Info.biWidth, Info.biBitCount, 32) * Cardinal(abs(Info.biHeight));
end;
{$IFDEF O_PLUS}
  {$O+}
  {$UNDEF O_PLUS}
{$ENDIF}

// -------------------
// InternalGetDIBSizes
// -------------------
// Calculates the buffer sizes nescessary for convertion of a bitmap to a DIB
// of a specified PixelFormat.
// See the GetDIBSizes API function for more info.
//
// Parameters:
// Bitmap	The handle of the source bitmap.
// InfoHeaderSize
//		The returned size of a buffer that will receive the DIB's
//		TBitmapInfo structure.
// ImageSize	The returned size of a buffer that will receive the DIB's
//		pixel data.
// PixelFormat	The pixel format of the destination DIB.
//
procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: Integer;
  var ImageSize: longInt; PixelFormat: TPixelFormat);
// From graphics.pas, "optimized" for our use
var
  Info		: TBitmapInfoHeader;
begin
  InitializeBitmapInfoHeader(Bitmap, Info, PixelFormat);
  // Check for palette device format
  if (Info.biBitCount > 8) then
  begin
    // Header but no palette
    InfoHeaderSize := SizeOf(TBitmapInfoHeader);
    if ((Info.biCompression and BI_BITFIELDS) <> 0) then
      Inc(InfoHeaderSize, 12);
  end else
    // Header and palette
    InfoHeaderSize := SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) * (1 shl Info.biBitCount);
  ImageSize := Info.biSizeImage;
end;

// --------------
// InternalGetDIB
// --------------
// Converts a bitmap to a DIB of a specified PixelFormat.
//
// Parameters:
// Bitmap	The handle of the source bitmap.
// Pal		The handle of the source palette.
// BitmapInfo	The buffer that will receive the DIB's TBitmapInfo structure.
//		A buffer of sufficient size must have been allocated prior to
//		calling this function.
// Bits		The buffer that will receive the DIB's pixel data.
//		A buffer of sufficient size must have been allocated prior to
//		calling this function.
// PixelFormat	The pixel format of the destination DIB.
//
// Returns:
// True on success, False on failure.
//
// Note: The InternalGetDIBSizes function can be used to calculate the
// nescessary sizes of the BitmapInfo and Bits buffers.
//
function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
  var BitmapInfo; var Bits; PixelFormat: TPixelFormat): Boolean;
// From graphics.pas, "optimized" for our use
var
  OldPal	: HPALETTE;
  DC		: HDC;
begin
  InitializeBitmapInfoHeader(Bitmap, TBitmapInfoHeader(BitmapInfo), PixelFormat);
  OldPal := 0;
  DC := CreateCompatibleDC(0);
  try
    if (Palette <> 0) then
    begin
      OldPal := SelectPalette(DC, Palette, False);
      RealizePalette(DC);
    end;
    Result := (GetDIBits(DC, Bitmap, 0, abs(TBitmapInfoHeader(BitmapInfo).biHeight),
      @Bits, TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) <> 0);
  finally
    if (OldPal <> 0) then
      SelectPalette(DC, OldPal, False);
    DeleteDC(DC);
  end;
end;

// ----------
// DIBFromBit
// ----------
// Converts a bitmap to a DIB of a specified PixelFormat.
// The DIB is returned in a TMemoryStream ready for streaming to a BMP file.
//
// Note: As opposed to D2's DIBFromBit function, the returned stream also
// contains a TBitmapFileHeader at offset 0.
//
// Parameters:
// Stream	The TMemoryStream used to store the bitmap data.
//		The stream must be allocated and freed by the caller prior to
//		calling this function.
// Src		The handle of the source bitmap.
// Pal		The handle of the source palette.
// PixelFormat	The pixel format of the destination DIB.
// DIBHeader	A pointer to the DIB's TBitmapInfo (or TBitmapInfoHeader)
//		structure in the memory stream.
//		The size of the structure can either be deduced from the
//		pixel format (i.e. number of colors) or calculated by
//		subtracting the DIBHeader pointer from the DIBBits pointer.
// DIBBits	A pointer to the DIB's pixel data in the memory stream.
//
procedure DIBFromBit(Stream: TMemoryStream; Src: HBITMAP;
  Pal: HPALETTE; PixelFormat: TPixelFormat; var DIBHeader, DIBBits: Pointer);
// (From D2 graphics.pas, "optimized" for our use)
var
  HeaderSize		: integer;
  FileSize		: longInt;
  ImageSize		: longInt;
  BitmapFileHeader	: PBitmapFileHeader;
begin
  if (Src = 0) then
    Error(sInvalidBitmap);
  // Get header- and pixel data size for new pixel format
  InternalGetDIBSizes(Src, HeaderSize, ImageSize, PixelFormat);
  // Make room in stream for a TBitmapInfo and pixel data
  FileSize := sizeof(TBitmapFileHeader) + HeaderSize + ImageSize;
  Stream.SetSize(FileSize);
  // Get pointer to TBitmapFileHeader
  BitmapFileHeader := Stream.Memory;
  // Get pointer to TBitmapInfo
  DIBHeader := Pointer(Longint(BitmapFileHeader) + sizeof(TBitmapFileHeader));
  // Get pointer to pixel data
  DIBBits := Pointer(Longint(DIBHeader) + HeaderSize);
  // Initialize file header
  FillChar(BitmapFileHeader^, sizeof(TBitmapFileHeader), 0);
  with BitmapFileHeader^ do
  begin
    bfType := $4D42; // 'BM' = Windows BMP signature
    bfSize := FileSize; // File size (not needed)
    bfOffBits := sizeof(TBitmapFileHeader) + HeaderSize; // Offset of pixel data
  end;
  // Get pixel data in new pixel format
  InternalGetDIB(Src, Pal, DIBHeader^, DIBBits^, PixelFormat);
end;

// --------------
// GetPixelFormat
// --------------
// Returns the current pixel format of a bitmap.
//
// Replacement for delphi 3 TBitmap.PixelFormat getter.
//
// Parameters:
// Bitmap	The bitmap which pixel format is returned.
//
// Returns:
// The PixelFormat of the bitmap
//
function GetPixelFormat(Bitmap: TBitmap): TPixelFormat;
{$IFDEF VER9x}
// From graphics.pas, "optimized" for our use
var
  DIBSection		: TDIBSection;
  Bytes			: Integer;
  Handle		: HBitmap;
begin
  Result := pfCustom; // This value is never returned
  // BAD_STACK_ALIGNMENT 
  // Note: To work around an optimizer bug, we do not use Bitmap.Handle
  // directly. Instead we store the value and use it indirectly. Unless we do
  // this, the register containing Bitmap.Handle will be overwritten!
  Handle := Bitmap.Handle;
  if (Handle <> 0) then
  begin
    Bytes := GetObject(Handle, SizeOf(DIBSection), @DIBSection);
    if (Bytes = 0) then
      Error(sInvalidBitmap);

    with (DIBSection) do
    begin
      // Check for NT bitmap
      if (Bytes < (SizeOf(dsbm) + SizeOf(dsbmih))) or (dsbmih.biSize < SizeOf(dsbmih)) then
        DIBSection.dsBmih.biBitCount := dsbm.bmBitsPixel * dsbm.bmPlanes;

      case (dsBmih.biBitCount) of
        0: Result := pfDevice;
        1: Result := pf1bit;
        4: Result := pf4bit;
        8: Result := pf8bit;
        16: case (dsBmih.biCompression) of
              BI_RGB:
                Result := pf15Bit;
              BI_BITFIELDS:
                if (dsBitFields[1] = $07E0) then
                  Result := pf16Bit;
            end;
        24: Result := pf24Bit;
        32: if (dsBmih.biCompression = BI_RGB) then
              Result := pf32Bit;
      else
        Error(sUnsupportedBitmap);
      end;
    end;
  end else
//    Result := pfDevice;
    Error(sUnsupportedBitmap);
end;
{$ELSE}
begin
  Result := Bitmap.PixelFormat;
end;
{$ENDIF}

// --------------
// SetPixelFormat
// --------------
// Changes the pixel format of a TBitmap.
//
// Replacement for delphi 3 TBitmap.PixelFormat setter.
// The returned TBitmap will always be a DIB.
//
// Note: Under Delphi 3.x this function will leak a palette handle each time it
//       converts a TBitmap to pf8bit format!
//       If possible, use SafeSetPixelFormat instead to avoid this.
//
// Parameters:
// Bitmap	The bitmap to modify.
// PixelFormat	The pixel format to convert to.
//
procedure SetPixelFormat(Bitmap: TBitmap; PixelFormat: TPixelFormat);
{$IFDEF VER9x}
var
  Stream	: TMemoryStream;
  Header	,
  Bits		: Pointer;
begin
  // Can't change anything without a handle
  if (Bitmap.Handle = 0) then
    Error(sInvalidBitmap);

  // Only convert to supported formats
  if not(PixelFormat in SupportedPixelformats) then
    Error(sInvalidPixelFormat);

  // No need to convert to same format
  if (GetPixelFormat(Bitmap) = PixelFormat) then
    exit;

  Stream := TMemoryStream.Create;
  try
    // Convert to DIB file in memory stream
    DIBFromBit(Stream, Bitmap.Handle, Bitmap.Palette, PixelFormat, Header, Bits);
    // Load DIB from stream
    Stream.Position := 0;
    Bitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;
{$ELSE}
begin
  Bitmap.PixelFormat := PixelFormat;
end;
{$ENDIF}

{$IFDEF VER100}
var
  pf8BitBitmap: TBitmap = nil;
{$ENDIF}

// ------------------
// SafeSetPixelFormat
// ------------------
// Changes the pixel format of a TBitmap but doesn't preserve the contents.
//
// Replacement for Delphi 3 TBitmap.PixelFormat setter.
// The returned TBitmap will always be an empty DIB of the same size as the
// original bitmap.
//
// This function is used to avoid the palette handle leak that Delphi 3's
// SetPixelFormat and TBitmap.PixelFormat suffers from.
//
// Parameters:
// Bitmap	The bitmap to modify.
// PixelFormat	The pixel format to convert to.
//
procedure SafeSetPixelFormat(Bitmap: TBitmap; PixelFormat: TPixelFormat);
{$IFDEF VER9x}
begin
  SetPixelFormat(Bitmap, PixelFormat);
end;
{$ELSE}
{$IFNDEF VER100}
var
  Palette		: hPalette;
begin
  Bitmap.PixelFormat := PixelFormat;

  // Work around a bug in TBitmap:
  // When converting to pf8bit format, the palette assigned to TBitmap.Palette
  // will be a half tone palette (which only contains the 20 system colors).
  // Unfortunately this is not the palette used to render the bitmap and it
  // is also not the palette saved with the bitmap.
  if (PixelFormat = pf8bit) then
  begin
    // Disassociate the wrong palette from the bitmap (without affecting
    // the DIB color table)
    Palette := Bitmap.ReleasePalette;
    if (Palette <> 0) then
      DeleteObject(Palette);
    // Recreate the palette from the DIB color table
    Bitmap.Palette;
  end;
end;
{$ELSE}
var
  Width			,
  Height		: integer;
begin
  if (PixelFormat = pf8bit) then
  begin
    // Partial solution to "TBitmap.PixelFormat := pf8bit" leak
    // by Greg Chapman <glc@well.com>
    if (pf8BitBitmap = nil) then
    begin
      // Create a "template" bitmap
      // The bitmap is deleted in the finalization section of the unit.
      pf8BitBitmap:= TBitmap.Create;
      // Convert template to pf8bit format
      // This will leak 1 palette handle, but only once
      pf8BitBitmap.PixelFormat:= pf8Bit;
    end;
    // Store the size of the original bitmap
    Width := Bitmap.Width;
    Height := Bitmap.Height;
    // Convert to pf8bit format by copying template
    Bitmap.Assign(pf8BitBitmap);
    // Restore the original size
    Bitmap.Width := Width;
    Bitmap.Height := Height;
  end else
    // This is safe since only pf8bit leaks
    Bitmap.PixelFormat := PixelFormat;
end;
{$ENDIF}
{$ENDIF}


{$IFDEF VER9x}

// -----------
// CopyPalette
// -----------
// Copies a HPALETTE.
//
// Copied from D3 graphics.pas.
// This is declared private in some old versions of Delphi 2 so we have to
// implement it here to support those old versions.
//
// Parameters:
// Palette	The palette to copy.
//
// Returns:
// The handle to a new palette.
//
function CopyPalette(Palette: HPALETTE): HPALETTE;
var
  PaletteSize: Integer;
  LogPal: TMaxLogPalette;
begin
  Result := 0;
  if Palette = 0 then Exit;
  PaletteSize := 0;
  if GetObject(Palette, SizeOf(PaletteSize), @PaletteSize) = 0 then Exit;
  if PaletteSize = 0 then Exit;
  with LogPal do
  begin
    palVersion := $0300;
    palNumEntries := PaletteSize;
    GetPaletteEntries(Palette, 0, PaletteSize, palPalEntry);
  end;
  Result := CreatePalette(PLogPalette(@LogPal)^);
end;


// TThreadList implementation from Delphi 3 classes.pas
constructor TThreadList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
  FList := TList.Create;
end;

destructor TThreadList.Destroy;
begin
  LockList;    // Make sure nobody else is inside the list.
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    DeleteCriticalSection(FLock);
  end;
end;

procedure TThreadList.Add(Item: Pointer);
begin
  LockList;
  try
    if FList.IndexOf(Item) = -1 then
      FList.Add(Item);
  finally
    UnlockList;
  end;
end;

procedure TThreadList.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function  TThreadList.LockList: TList;
begin
  EnterCriticalSection(FLock);
  Result := FList;
end;

procedure TThreadList.Remove(Item: Pointer);
begin
  LockList;
  try
    FList.Remove(Item);
  finally
    UnlockList;
  end;
end;

procedure TThreadList.UnlockList;
begin
  LeaveCriticalSection(FLock);
end;
// End of TThreadList implementation

// From Delphi 3 sysutils.pas
{ CompareMem performs a binary compare of Length bytes of memory referenced
  by P1 to that of P2.  CompareMem returns True if the memory referenced by
  P1 is identical to that of P2. }
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,1
        SHR     ECX,1
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;

// Dummy ASSERT procedure since ASSERT does not exist in Delphi 2.x
procedure ASSERT(Condition: boolean; Message: string);
begin
end;

{$ENDIF} // Delphi 2.x stuff

////////////////////////////////////////////////////////////////////////////////
//
//			TDIB Classes
//
//  These classes gives read and write access to TBitmap's pixel data
//  independently of the Delphi version used.
//
////////////////////////////////////////////////////////////////////////////////
type
  TDIB = class(TObject)
  private
    FBitmap		: TBitmap;
    FPixelFormat	: TPixelFormat;
  protected
    function GetScanline(Row: integer): pointer; virtual; abstract;
    constructor Create(ABitmap: TBitmap; APixelFormat: TPixelFormat);
  public
    property Scanline[Row: integer]: pointer read GetScanline;
    property Bitmap: TBitmap read FBitmap;
    property PixelFormat: TPixelFormat read FPixelFormat;
  end;

  TDIBReader = class(TDIB)
  private
{$ifdef VER9x}
    FDIB		: TDIBSection;
    FDC			: HDC;
    FScanLine		: pointer;
    FLastRow		: integer;
    FInfo		: PBitmapInfo;
    FBytes		: integer;
{$endif}
  protected
    function GetScanline(Row: integer): pointer; override;
  public
    constructor Create(ABitmap: TBitmap; APixelFormat: TPixelFormat);
    destructor Destroy; override;
  end;

  TDIBWriter = class(TDIB)
  private
{$ifdef PIXELFORMAT_TOO_SLOW}
    FDIBInfo		: PBitmapInfo;
    FDIBBits		: pointer;
    FDIBInfoSize	: integer;
    FDIBBitsSize	: longInt;
{$ifndef CREATEDIBSECTION_SLOW}
    FDIB		: HBITMAP;
{$endif}
{$endif}
    FPalette		: HPalette;
    FHeight		: integer;
    FWidth		: integer;
  protected
    procedure CreateDIB;
    procedure FreeDIB;
    procedure NeedDIB;
    function GetScanline(Row: integer): pointer; override;
  public
    constructor Create(ABitmap: TBitmap; APixelFormat: TPixelFormat;
      AWidth, AHeight: integer; APalette: HPalette);
    destructor Destroy; override;
    procedure UpdateBitmap;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property Palette: HPalette read FPalette;
  end;

////////////////////////////////////////////////////////////////////////////////
constructor TDIB.Create(ABitmap: TBitmap; APixelFormat: TPixelFormat);
begin
  inherited Create;
  FBitmap := ABitmap;
  FPixelFormat := APixelFormat;
end;

////////////////////////////////////////////////////////////////////////////////
constructor TDIBReader.Create(ABitmap: TBitmap; APixelFormat: TPixelFormat);
{$ifdef VER9x}
var
  InfoHeaderSize	: integer;
  ImageSize		: longInt;
{$endif}
begin
  inherited Create(ABitmap, APixelFormat);
{$ifndef VER9x}
  SetPixelFormat(FBitmap, FPixelFormat);
{$else}
  FDC := CreateCompatibleDC(0);
  SelectPalette(FDC, FBitmap.Palette, False);

  // Allocate DIB info structure
  InternalGetDIBSizes(ABitmap.Handle, InfoHeaderSize, ImageSize, APixelFormat);
  GetMem(FInfo, InfoHeaderSize);
  // Get DIB info
  InitializeBitmapInfoHeader(ABitmap.Handle, FInfo^.bmiHeader, APixelFormat);

  // Allocate scan line buffer
  GetMem(FScanLine, ImageSize DIV abs(FInfo^.bmiHeader.biHeight));

  FLastRow := -1;
{$endif}
end;

destructor TDIBReader.Destroy;
begin
{$ifdef VER9x}
  DeleteDC(FDC);
  FreeMem(FScanLine);
  FreeMem(FInfo);
{$endif}
  inherited Destroy;
end;

function TDIBReader.GetScanline(Row: integer): pointer;
begin
{$ifdef VER9x}
  if (Row < 0) or (Row >= FBitmap.Height) then
    raise EInvalidGraphicOperation.Create(SScanLine);
  GDIFlush;

  Result := FScanLine;
  if (Row = FLastRow) then
    exit;
  FLastRow := Row;

  if (FInfo^.bmiHeader.biHeight > 0) then  // bottom-up DIB
    Row := FInfo^.bmiHeader.biHeight - Row - 1;
  GetDIBits(FDC, FBitmap.Handle, Row, 1, FScanLine, FInfo^, DIB_RGB_COLORS);

{$else}
  Result := FBitmap.ScanLine[Row];
{$endif}
end;

////////////////////////////////////////////////////////////////////////////////
constructor TDIBWriter.Create(ABitmap: TBitmap; APixelFormat: TPixelFormat;
  AWidth, AHeight: integer; APalette: HPalette);
begin
  inherited Create(ABitmap, APixelFormat);

  // DIB writer only supports 8 or 24 bit bitmaps
  if not(APixelFormat in [pf8bit, pf24bit]) then
    Error(sInvalidPixelFormat);
  if (AWidth = 0) or (AHeight = 0) then
    Error(sBadDimension);

  FHeight := AHeight;
  FWidth := AWidth;
{$ifndef PIXELFORMAT_TOO_SLOW}
  FBitmap.Palette := 0;
  FBitmap.Height := FHeight;
  FBitmap.Width := FWidth;
  SafeSetPixelFormat(FBitmap, FPixelFormat);
  FPalette := CopyPalette(APalette);
  FBitmap.Palette := FPalette;
{$else}
  FPalette := APalette;
  FDIBInfo := nil;
  FDIBBits := nil;
{$ifndef CREATEDIBSECTION_SLOW}
  FDIB := 0;
{$endif}
{$endif}
end;

destructor TDIBWriter.Destroy;
begin
  UpdateBitmap;
  FreeDIB;
  inherited Destroy;
end;

function TDIBWriter.GetScanline(Row: integer): pointer;
begin
{$ifdef PIXELFORMAT_TOO_SLOW}
  NeedDIB;

  if (FDIBBits = nil) then
    Error(sNoDIB);
  with FDIBInfo^.bmiHeader do
  begin
    if (Row < 0) or (Row >= Height) then
      raise EInvalidGraphicOperation.Create(SScanLine);
    GDIFlush;

    if biHeight > 0 then  // bottom-up DIB
      Row := biHeight - Row - 1;
    Result := PChar(Cardinal(FDIBBits) + Cardinal(Row) * AlignBit(biWidth, biBitCount, 32));
  end;
{$else}
  Result := FBitmap.ScanLine[Row];
{$endif}
end;

procedure TDIBWriter.CreateDIB;
{$IFDEF PIXELFORMAT_TOO_SLOW}
var
  SrcColors		: WORD;
//  ScreenDC		: HDC;

  // From Delphi 3.02 graphics.pas
  // There is a bug in the ByteSwapColors from Delphi 3.0!
  procedure ByteSwapColors(var Colors; Count: Integer);
  var   // convert RGB to BGR and vice-versa.  TRGBQuad <-> TPaletteEntry
    SysInfo: TSystemInfo;
  begin
    GetSystemInfo(SysInfo);
    asm
          MOV   EDX, Colors
          MOV   ECX, Count
          DEC   ECX
          JS    @@END
          LEA   EAX, SysInfo
          CMP   [EAX].TSystemInfo.wProcessorLevel, 3
          JE    @@386
    @@1:  MOV   EAX, [EDX+ECX*4]
          BSWAP EAX
          SHR   EAX,8
          MOV   [EDX+ECX*4],EAX
          DEC   ECX
          JNS   @@1
          JMP   @@END
    @@386:
          PUSH  EBX
    @@2:  XOR   EBX,EBX
          MOV   EAX, [EDX+ECX*4]
          MOV   BH, AL
          MOV   BL, AH
          SHR   EAX,16
          SHL   EBX,8
          MOV   BL, AL
          MOV   [EDX+ECX*4],EBX
          DEC   ECX
          JNS   @@2
          POP   EBX
      @@END:
    end;
  end;
{$ENDIF}
begin
{$ifdef PIXELFORMAT_TOO_SLOW}
  FreeDIB;

  if (PixelFormat = pf8bit) then
    // 8 bit: Header and palette
    FDIBInfoSize := SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) * (1 shl 8)
  else
    // 24 bit: Header but no palette
    FDIBInfoSize := SizeOf(TBitmapInfoHeader);

  // Allocate TBitmapInfo structure
  GetMem(FDIBInfo, FDIBInfoSize);
  try
    FDIBInfo^.bmiHeader.biSize := SizeOf(FDIBInfo^.bmiHeader);
    FDIBInfo^.bmiHeader.biWidth := Width;
    FDIBInfo^.bmiHeader.biHeight := Height;
    FDIBInfo^.bmiHeader.biPlanes := 1;
    FDIBInfo^.bmiHeader.biSizeImage := 0;
    FDIBInfo^.bmiHeader.biCompression := BI_RGB;

    if (PixelFormat = pf8bit) then
    begin
      FDIBInfo^.bmiHeader.biBitCount := 8;
      // Find number of colors defined by palette
      if (Palette <> 0) and
        (GetObject(Palette, sizeof(SrcColors), @SrcColors) <> 0) and
        (SrcColors <> 0) then
      begin
        // Copy all colors...
        GetPaletteEntries(Palette, 0, SrcColors, FDIBInfo^.bmiColors[0]);
        // ...and convert BGR to RGB
        ByteSwapColors(FDIBInfo^.bmiColors[0], SrcColors);
      end else
        SrcColors := 0;

      // Finally zero any unused entried
      if (SrcColors < 256) then
        FillChar(pointer(LongInt(@FDIBInfo^.bmiColors)+SizeOf(TRGBQuad)*SrcColors)^,
          256 - SrcColors, 0);
      FDIBInfo^.bmiHeader.biClrUsed := 256;
      FDIBInfo^.bmiHeader.biClrImportant := SrcColors;
    end else
    begin
      FDIBInfo^.bmiHeader.biBitCount := 24;
      FDIBInfo^.bmiHeader.biClrUsed := 0;
      FDIBInfo^.bmiHeader.biClrImportant := 0;
    end;
    FDIBBitsSize := AlignBit(Width, FDIBInfo^.bmiHeader.biBitCount, 32) * Cardinal(abs(Height));

{$ifdef CREATEDIBSECTION_SLOW}
    FDIBBits := GlobalAllocPtr(GMEM_MOVEABLE, FDIBBitsSize);
    if (FDIBBits = nil) then
      raise EOutOfMemory.Create(sOutOfMemDIB);
{$else}
//    ScreenDC := GDICheck(GetDC(0));
    try
      // Allocate DIB section
      // Note: You can ignore warnings about the HDC parameter being 0. The
      // parameter is not used for 24 bit bitmaps
      FDIB := GDICheck(CreateDIBSection(0 {ScreenDC}, FDIBInfo^, DIB_RGB_COLORS,
        FDIBBits,
        {$IFDEF VER9x} nil, {$ELSE} 0, {$ENDIF}
        0));
    finally
//      ReleaseDC(0, ScreenDC);
    end;
{$endif}

  except
    FreeDIB;
    raise;
  end;
{$endif}
end;

procedure TDIBWriter.FreeDIB;
begin
{$ifdef PIXELFORMAT_TOO_SLOW}
  if (FDIBInfo <> nil) then
    FreeMem(FDIBInfo);
{$ifdef CREATEDIBSECTION_SLOW}
  if (FDIBBits <> nil) then
    GlobalFreePtr(FDIBBits);
{$else}
  if (FDIB <> 0) then
    DeleteObject(FDIB);
  FDIB := 0;
{$endif}
  FDIBInfo := nil;
  FDIBBits := nil;
{$endif}
end;

procedure TDIBWriter.NeedDIB;
begin
{$ifdef PIXELFORMAT_TOO_SLOW}
{$ifdef CREATEDIBSECTION_SLOW}
  if (FDIBBits = nil) then
{$else}
  if (FDIB = 0) then
{$endif}
    CreateDIB;
{$endif}
end;

// Convert the DIB created by CreateDIB back to a TBitmap
procedure TDIBWriter.UpdateBitmap;
{$ifdef PIXELFORMAT_TOO_SLOW}
var
  Stream		: TMemoryStream;
  FileSize		: longInt;
  BitmapFileHeader	: TBitmapFileHeader;
{$endif}
begin
{$ifdef PIXELFORMAT_TOO_SLOW}

{$ifdef CREATEDIBSECTION_SLOW}
  if (FDIBBits = nil) then
{$else}
  if (FDIB = 0) then
{$endif}
    exit;

  // Win95 and NT differs in what solution performs best
{$ifndef CREATEDIBSECTION_SLOW}
{$ifdef VER10_PLUS}
  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    // Assign DIB to bitmap
    FBitmap.Handle := FDIB;
    FDIB := 0;
    FBitmap.Palette := CopyPalette(Palette);
  end else
{$endif}
{$endif}
  begin
    // Write DIB to a stream in the BMP file format
    Stream := TMemoryStream.Create;
    try
      // Make room in stream for a TBitmapInfo and pixel data
      FileSize := sizeof(TBitmapFileHeader) + FDIBInfoSize + FDIBBitsSize;
      Stream.SetSize(FileSize);
      // Initialize file header
      FillChar(BitmapFileHeader, sizeof(TBitmapFileHeader), 0);
      with BitmapFileHeader do
      begin
        bfType := $4D42; // 'BM' = Windows BMP signature
        bfSize := FileSize; // File size (not needed)
        bfOffBits := sizeof(TBitmapFileHeader) + FDIBInfoSize; // Offset of pixel data
      end;
      // Save file header
      Stream.Write(BitmapFileHeader, sizeof(TBitmapFileHeader));
      // Save TBitmapInfo structure
      Stream.Write(FDIBInfo^, FDIBInfoSize);
      // Save pixel data
      Stream.Write(FDIBBits^, FDIBBitsSize);

      // Rewind and load bitmap from stream
      Stream.Position := 0;
      FBitmap.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
{$endif}
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Color Mapping
//
////////////////////////////////////////////////////////////////////////////////
type
  TColorLookup = class(TObject)
  private
    FColors		: integer;
  public
    constructor Create(Palette: hPalette); virtual;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; virtual; abstract;
    property Colors: integer read FColors;
  end;

  PRGBQuadArray = ^TRGBQuadArray;		// From Delphi 3 graphics.pas
  TRGBQuadArray = array[Byte] of TRGBQuad;	// From Delphi 3 graphics.pas

  BGRArray = array[0..0] of TRGBTriple;
  PBGRArray = ^BGRArray;

  PalArray =  array[byte] of TPaletteEntry;
  PPalArray = ^PalArray;

  // TFastColorLookup implements a simple but reasonably fast generic color
  // mapper. It trades precision for speed by reducing the size of the color
  // space.
  // Using a class instead of inline code results in a speed penalty of
  // approx. 15% but reduces the complexity of the color reduction routines that
  // uses it. If bitmap to GIF conversion speed is really important to you, the
  // implementation can easily be inlined again.
  TInverseLookup = array[0..1 SHL 15-1] of SmallInt;
  PInverseLookup = ^TInverseLookup;

  TFastColorLookup = class(TColorLookup)
  private
    FPaletteEntries	: PPalArray;
    FInverseLookup	: PInverseLookup;
  public
    constructor Create(Palette: hPalette); override;
    destructor Destroy; override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
  end;

  // TSlowColorLookup implements a precise but very slow generic color mapper.
  // It uses the GetNearestPaletteIndex GDI function.
  // Note: Tests has shown TFastColorLookup to be more precise than
  // TSlowColorLookup in many cases. I can't explain why...
  TSlowColorLookup = class(TColorLookup)
  private
    FPaletteEntries	: PPalArray;
    FPalette		: hPalette;
  public
    constructor Create(Palette: hPalette); override;
    destructor Destroy; override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
  end;

  // TNetscapeColorLookup maps colors to the netscape 6*6*6 color cube.
  TNetscapeColorLookup = class(TColorLookup)
  public
    constructor Create(Palette: hPalette); override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
  end;

  // TGrayWindowsLookup maps colors to 4 shade palette.
  TGrayWindowsLookup = class(TSlowColorLookup)
  public
    constructor Create(Palette: hPalette); override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
  end;

  // TGrayScaleLookup maps colors to a uniform 256 shade palette.
  TGrayScaleLookup = class(TColorLookup)
  public
    constructor Create(Palette: hPalette); override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
  end;

  // TMonochromeLookup maps colors to a black/white palette.
  TMonochromeLookup = class(TColorLookup)
  public
    constructor Create(Palette: hPalette); override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
  end;

constructor TColorLookup.Create(Palette: hPalette);
begin
  inherited Create;
end;

constructor TFastColorLookup.Create(Palette: hPalette);
var
  i			: integer;
  InverseIndex		: integer;
begin
  inherited Create(Palette);

  GetMem(FPaletteEntries, sizeof(TPaletteEntry) * 256);
  FColors := GetPaletteEntries(Palette, 0, 256, FPaletteEntries^);

  New(FInverseLookup);
  for i := low(TInverseLookup) to high(TInverseLookup) do
    FInverseLookup^[i] := -1;

  // Premap palette colors
  if (FColors > 0) then
    for i := 0 to FColors-1 do
      with FPaletteEntries^[i] do
      begin
        InverseIndex := (peRed SHR 3) OR ((peGreen AND $F8) SHL 2) OR ((peBlue AND $F8) SHL 7);
        if (FInverseLookup^[InverseIndex] = -1) then
          FInverseLookup^[InverseIndex] := i;
      end;
end;

destructor TFastColorLookup.Destroy;
begin
  if (FPaletteEntries <> nil) then
    FreeMem(FPaletteEntries);
  if (FInverseLookup <> nil) then
    Dispose(FInverseLookup);

  inherited Destroy;
end;

// Map color to arbitrary palette
function TFastColorLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
var
  i			: integer;
  InverseIndex		: integer;
  Delta			,
  MinDelta		,
  MinColor		: integer;
begin
  // Reduce color space with 3 bits in each dimension
  InverseIndex := (Red SHR 3) OR ((Green AND $F8) SHL 2) OR ((Blue AND $F8) SHL 7);

  if (FInverseLookup^[InverseIndex] <> -1) then
    Result := char(FInverseLookup^[InverseIndex])
  else
  begin
    // Sequential scan for nearest color to minimize euclidian distance
    MinDelta := 3 * (256 * 256);
    MinColor := 0;
    for i := 0 to FColors-1 do
      with FPaletteEntries[i] do
      begin
        Delta := ABS(peRed - Red) + ABS(peGreen - Green) + ABS(peBlue - Blue);
        if (Delta < MinDelta) then
        begin
          MinDelta := Delta;
          MinColor := i;
        end;
      end;
    Result := char(MinColor);
    FInverseLookup^[InverseIndex] := MinColor;
  end;

  with FPaletteEntries^[ord(Result)] do
  begin
    R := peRed;
    G := peGreen;
    B := peBlue;
  end;
end;

constructor TSlowColorLookup.Create(Palette: hPalette);
begin
  inherited Create(Palette);
  FPalette := Palette;
  FColors := GetPaletteEntries(Palette, 0, 256, nil^);
  if (FColors > 0) then
  begin
    GetMem(FPaletteEntries, sizeof(TPaletteEntry) * FColors);
    FColors := GetPaletteEntries(Palette, 0, 256, FPaletteEntries^);
  end;
end;

destructor TSlowColorLookup.Destroy;
begin
  if (FPaletteEntries <> nil) then
    FreeMem(FPaletteEntries);

  inherited Destroy;
end;

// Map color to arbitrary palette
function TSlowColorLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
begin
  Result := char(GetNearestPaletteIndex(FPalette, Red OR (Green SHL 8) OR (Blue SHL 16)));
  if (FPaletteEntries <> nil) then
    with FPaletteEntries^[ord(Result)] do
    begin
      R := peRed;
      G := peGreen;
      B := peBlue;
    end;
end;

constructor TNetscapeColorLookup.Create(Palette: hPalette);
begin
  inherited Create(Palette);
  FColors := 6*6*6; // This better be true or something is wrong
end;

// Map color to netscape 6*6*6 color cube
function TNetscapeColorLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
begin
  R := (Red+3) DIV 51;
  G := (Green+3) DIV 51;
  B := (Blue+3) DIV 51;
  Result := char(B + 6*G + 36*R);
  R := R * 51;
  G := G * 51;
  B := B * 51;
end;

constructor TGrayWindowsLookup.Create(Palette: hPalette);
begin
  inherited Create(Palette);
  FColors := 4;
end;

// Convert color to windows grays
function TGrayWindowsLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
begin
  Result := inherited Lookup(MulDiv(Red, 77, 256),
    MulDiv(Green, 150, 256), MulDiv(Blue, 29, 256), R, G, B);
end;

constructor TGrayScaleLookup.Create(Palette: hPalette);
begin
  inherited Create(Palette);
  FColors := 256;
end;

// Convert color to grayscale
function TGrayScaleLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
begin
  Result := char((Blue*29 + Green*150 + Red*77) DIV 256);
  R := ord(Result);
  G := ord(Result);
  B := ord(Result);
end;

constructor TMonochromeLookup.Create(Palette: hPalette);
begin
  inherited Create(Palette);
  FColors := 2;
end;

// Convert color to black/white
function TMonochromeLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
begin
  if ((Blue*29 + Green*150 + Red*77) > 32512) then
  begin
    Result := #1;
    R := 255;
    G := 255;
    B := 255;
  end else
  begin
    Result := #0;
    R := 0;
    G := 0;
    B := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Dithering engine
//
////////////////////////////////////////////////////////////////////////////////
type
  TDitherEngine = class
  private
  protected
    FDirection		: integer;
    FColumn		: integer;
    FLookup		: TColorLookup;
    Width		: integer;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); virtual;
    function Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; virtual;
    procedure NextLine; virtual;
    procedure NextColumn;

    property Direction: integer read FDirection;
    property Column: integer read FColumn;
  end;

  // Note: TErrorTerm does only *need* to be 16 bits wide, but since
  // it is *much* faster to use native machine words (32 bit), we sacrifice
  // some bytes (a lot actually) to improve performance.
  TErrorTerm		= Integer;
  TErrors		= array[0..0] of TErrorTerm;
  PErrors		= ^TErrors;

  TFloydSteinbergDitherer = class(TDitherEngine)
  private
    ErrorsR		,
    ErrorsG		,
    ErrorsB		: PErrors;
    ErrorR		,
    ErrorG		,
    ErrorB		: PErrors;
    CurrentErrorR	,		// Current error or pixel value
    CurrentErrorG	,
    CurrentErrorB	,
    BelowErrorR		,		// Error for pixel below current
    BelowErrorG		,
    BelowErrorB		,
    BelowPrevErrorR	,		// Error for pixel below previous pixel
    BelowPrevErrorG	,
    BelowPrevErrorB	: TErrorTerm;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); override;
    destructor Destroy; override;
    function Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
    procedure NextLine; override;
  end;

  T5by3Ditherer = class(TDitherEngine)
  private
    ErrorsR0		,
    ErrorsG0		,
    ErrorsB0		,
    ErrorsR1		,
    ErrorsG1		,
    ErrorsB1		,
    ErrorsR2		,
    ErrorsG2		,
    ErrorsB2		: PErrors;
    ErrorR0		,
    ErrorG0		,
    ErrorB0		,
    ErrorR1		,
    ErrorG1		,
    ErrorB1		,
    ErrorR2		,
    ErrorG2		,
    ErrorB2		: PErrors;
    FDirection2		: integer;
  protected
    FDivisor		: integer;
    procedure Propagate(Errors0, Errors1, Errors2: PErrors; Error: integer); virtual; abstract;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); override;
    destructor Destroy; override;
    function Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
    procedure NextLine; override;
  end;

  TStuckiDitherer = class(T5by3Ditherer)
  protected
    procedure Propagate(Errors0, Errors1, Errors2: PErrors; Error: integer); override;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); override;
  end;

  TSierraDitherer = class(T5by3Ditherer)
  protected
    procedure Propagate(Errors0, Errors1, Errors2: PErrors; Error: integer); override;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); override;
  end;

  TJaJuNiDitherer = class(T5by3Ditherer)
  protected
    procedure Propagate(Errors0, Errors1, Errors2: PErrors; Error: integer); override;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); override;
  end;

  TSteveArcheDitherer = class(TDitherEngine)
  private
    ErrorsR0		,
    ErrorsG0		,
    ErrorsB0		,
    ErrorsR1		,
    ErrorsG1		,
    ErrorsB1		,
    ErrorsR2		,
    ErrorsG2		,
    ErrorsB2		,
    ErrorsR3		,
    ErrorsG3		,
    ErrorsB3		: PErrors;
    ErrorR0		,
    ErrorG0		,
    ErrorB0		,
    ErrorR1		,
    ErrorG1		,
    ErrorB1		,
    ErrorR2		,
    ErrorG2		,
    ErrorB2		,
    ErrorR3		,
    ErrorG3		,
    ErrorB3		: PErrors;
    FDirection2		,
    FDirection3		: integer;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); override;
    destructor Destroy; override;
    function Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
    procedure NextLine; override;
  end;

  TBurkesDitherer = class(TDitherEngine)
  private
    ErrorsR0		,
    ErrorsG0		,
    ErrorsB0		,
    ErrorsR1		,
    ErrorsG1		,
    ErrorsB1		: PErrors;
    ErrorR0		,
    ErrorG0		,
    ErrorB0		,
    ErrorR1		,
    ErrorG1		,
    ErrorB1		: PErrors;
    FDirection2		: integer;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); override;
    destructor Destroy; override;
    function Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): char; override;
    procedure NextLine; override;
  end;

////////////////////////////////////////////////////////////////////////////////
//	TDitherEngine
constructor TDitherEngine.Create(AWidth: integer; Lookup: TColorLookup);
begin
  inherited Create;

  FLookup := Lookup;
  Width := AWidth;

  FDirection := 1;
  FColumn := 0;
end;

function TDitherEngine.Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
begin
  // Map color to palette
  Result := FLookup.Lookup(Red, Green, Blue, R, G, B);
  NextColumn;
end;

procedure TDitherEngine.NextLine;
begin
  FDirection := -FDirection;
  if (FDirection = 1) then
    FColumn := 0
  else
    FColumn := Width-1;
end;

procedure TDitherEngine.NextColumn;
begin
  inc(FColumn, FDirection);
end;

////////////////////////////////////////////////////////////////////////////////
//	TFloydSteinbergDitherer
constructor TFloydSteinbergDitherer.Create(AWidth: integer; Lookup: TColorLookup);
begin
  inherited Create(AWidth, Lookup);

  // The Error arrays has (columns + 2) entries; the extra entry at
  // each end saves us from special-casing the first and last pixels.
  // We can get away with a single array (holding one row's worth of errors)
  // by using it to store the current row's errors at pixel columns not yet
  // processed, but the next row's errors at columns already processed.  We
  // need only a few extra variables to hold the errors immediately around the
  // current column.  (If we are lucky, those variables are in registers, but
  // even if not, they're probably cheaper to access than array elements are.)
  GetMem(ErrorsR, sizeof(TErrorTerm)*(Width+2));
  GetMem(ErrorsG, sizeof(TErrorTerm)*(Width+2));
  GetMem(ErrorsB, sizeof(TErrorTerm)*(Width+2));
  FillChar(ErrorsR^, sizeof(TErrorTerm)*(Width+2), 0);
  FillChar(ErrorsG^, sizeof(TErrorTerm)*(Width+2), 0);
  FillChar(ErrorsB^, sizeof(TErrorTerm)*(Width+2), 0);
  ErrorR := ErrorsR;
  ErrorG := ErrorsG;
  ErrorB := ErrorsB;
  CurrentErrorR := 0;
  CurrentErrorG := CurrentErrorR;
  CurrentErrorB := CurrentErrorR;
  BelowErrorR := CurrentErrorR;
  BelowErrorG := CurrentErrorR;
  BelowErrorB := CurrentErrorR;
  BelowPrevErrorR := CurrentErrorR;
  BelowPrevErrorG := CurrentErrorR;
  BelowPrevErrorB := CurrentErrorR;
end;

destructor TFloydSteinbergDitherer.Destroy;
begin
  FreeMem(ErrorsR);
  FreeMem(ErrorsG);
  FreeMem(ErrorsB);
  inherited Destroy;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TFloydSteinbergDitherer.Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
var
  BelowNextError	: TErrorTerm;
  Delta			: TErrorTerm;
begin
  CurrentErrorR := Red + (CurrentErrorR + ErrorR[0] + 8) DIV 16;
//  CurrentErrorR := Red + (CurrentErrorR + ErrorR[Direction] + 8) DIV 16;
  if (CurrentErrorR < 0) then
    CurrentErrorR := 0
  else if (CurrentErrorR > 255) then
    CurrentErrorR := 255;

  CurrentErrorG := Green + (CurrentErrorG + ErrorG[0] + 8) DIV 16;
//  CurrentErrorG := Green + (CurrentErrorG + ErrorG[Direction] + 8) DIV 16;
  if (CurrentErrorG < 0) then
    CurrentErrorG := 0
  else if (CurrentErrorG > 255) then
    CurrentErrorG := 255;

  CurrentErrorB := Blue + (CurrentErrorB + ErrorB[0] + 8) DIV 16;
//  CurrentErrorB := Blue + (CurrentErrorB + ErrorB[Direction] + 8) DIV 16;
  if (CurrentErrorB < 0) then
    CurrentErrorB := 0
  else if (CurrentErrorB > 255) then
    CurrentErrorB := 255;

  // Map color to palette
  Result := inherited Dither(CurrentErrorR, CurrentErrorG, CurrentErrorB, R, G, B);

  // Propagate Floyd-Steinberg error terms.
  // Errors are accumulated into the error arrays, at a resolution of
  // 1/16th of a pixel count.  The error at a given pixel is propagated
  // to its not-yet-processed neighbors using the standard F-S fractions,
  //		...	(here)	7/16
  //		3/16	5/16	1/16
  // We work left-to-right on even rows, right-to-left on odd rows.

  // Red component
  CurrentErrorR := CurrentErrorR - R;
  if (CurrentErrorR <> 0) then
  begin
    BelowNextError := CurrentErrorR;			// Error * 1

    Delta := CurrentErrorR * 2;
    inc(CurrentErrorR, Delta);
    ErrorR[0] := BelowPrevErrorR + CurrentErrorR;	// Error * 3

    inc(CurrentErrorR, Delta);
    BelowPrevErrorR := BelowErrorR + CurrentErrorR;	// Error * 5

    BelowErrorR := BelowNextError;			// Error * 1

    inc(CurrentErrorR, Delta);				// Error * 7
  end;

  // Green component
  CurrentErrorG := CurrentErrorG - G;
  if (CurrentErrorG <> 0) then
  begin
    BelowNextError := CurrentErrorG;			// Error * 1

    Delta := CurrentErrorG * 2;
    inc(CurrentErrorG, Delta);
    ErrorG[0] := BelowPrevErrorG + CurrentErrorG;	// Error * 3

    inc(CurrentErrorG, Delta);
    BelowPrevErrorG := BelowErrorG + CurrentErrorG;	// Error * 5

    BelowErrorG := BelowNextError;			// Error * 1

    inc(CurrentErrorG, Delta);				// Error * 7
  end;

  // Blue component
  CurrentErrorB := CurrentErrorB - B;
  if (CurrentErrorB <> 0) then
  begin
    BelowNextError := CurrentErrorB;			// Error * 1

    Delta := CurrentErrorB * 2;
    inc(CurrentErrorB, Delta);
    ErrorB[0] := BelowPrevErrorB + CurrentErrorB;	// Error * 3

    inc(CurrentErrorB, Delta);
    BelowPrevErrorB := BelowErrorB + CurrentErrorB;	// Error * 5

    BelowErrorB := BelowNextError;			// Error * 1

    inc(CurrentErrorB, Delta);				// Error * 7
  end;

  // Move on to next column
  if (Direction = 1) then
  begin
    inc(longInt(ErrorR), sizeof(TErrorTerm));
    inc(longInt(ErrorG), sizeof(TErrorTerm));
    inc(longInt(ErrorB), sizeof(TErrorTerm));
  end else
  begin
    dec(longInt(ErrorR), sizeof(TErrorTerm));
    dec(longInt(ErrorG), sizeof(TErrorTerm));
    dec(longInt(ErrorB), sizeof(TErrorTerm));
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure TFloydSteinbergDitherer.NextLine;
begin
  ErrorR[0] := BelowPrevErrorR;
  ErrorG[0] := BelowPrevErrorG;
  ErrorB[0] := BelowPrevErrorB;

  // Note: The optimizer produces better code for this construct:
  //   a := 0; b := a; c := a;
  // compared to this construct:
  //   a := 0; b := 0; c := 0;
  CurrentErrorR := 0;
  CurrentErrorG := CurrentErrorR;
  CurrentErrorB := CurrentErrorG;
  BelowErrorR := CurrentErrorG;
  BelowErrorG := CurrentErrorG;
  BelowErrorB := CurrentErrorG;
  BelowPrevErrorR := CurrentErrorG;
  BelowPrevErrorG := CurrentErrorG;
  BelowPrevErrorB := CurrentErrorG;

  inherited NextLine;

  if (Direction = 1) then
  begin
    ErrorR := ErrorsR;
    ErrorG := ErrorsG;
    ErrorB := ErrorsB;
  end else
  begin
    ErrorR := @ErrorsR[Width+1];
    ErrorG := @ErrorsG[Width+1];
    ErrorB := @ErrorsB[Width+1];
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//	T5by3Ditherer
constructor T5by3Ditherer.Create(AWidth: integer; Lookup: TColorLookup);
begin
  inherited Create(AWidth, Lookup);

  GetMem(ErrorsR0, sizeof(TErrorTerm)*(Width+4));
  GetMem(ErrorsG0, sizeof(TErrorTerm)*(Width+4));
  GetMem(ErrorsB0, sizeof(TErrorTerm)*(Width+4));
  GetMem(ErrorsR1, sizeof(TErrorTerm)*(Width+4));
  GetMem(ErrorsG1, sizeof(TErrorTerm)*(Width+4));
  GetMem(ErrorsB1, sizeof(TErrorTerm)*(Width+4));
  GetMem(ErrorsR2, sizeof(TErrorTerm)*(Width+4));
  GetMem(ErrorsG2, sizeof(TErrorTerm)*(Width+4));
  GetMem(ErrorsB2, sizeof(TErrorTerm)*(Width+4));
  FillChar(ErrorsR0^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsG0^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsB0^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsR1^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsG1^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsB1^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsR2^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsG2^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsB2^, sizeof(TErrorTerm)*(Width+4), 0);

  FDivisor := 1;
  FDirection2 := 2 * Direction;
  ErrorR0 := PErrors(longInt(ErrorsR0)+2*sizeof(TErrorTerm));
  ErrorG0 := PErrors(longInt(ErrorsG0)+2*sizeof(TErrorTerm));
  ErrorB0 := PErrors(longInt(ErrorsB0)+2*sizeof(TErrorTerm));
  ErrorR1 := PErrors(longInt(ErrorsR1)+2*sizeof(TErrorTerm));
  ErrorG1 := PErrors(longInt(ErrorsG1)+2*sizeof(TErrorTerm));
  ErrorB1 := PErrors(longInt(ErrorsB1)+2*sizeof(TErrorTerm));
  ErrorR2 := PErrors(longInt(ErrorsR2)+2*sizeof(TErrorTerm));
  ErrorG2 := PErrors(longInt(ErrorsG2)+2*sizeof(TErrorTerm));
  ErrorB2 := PErrors(longInt(ErrorsB2)+2*sizeof(TErrorTerm));
end;

destructor T5by3Ditherer.Destroy;
begin
  FreeMem(ErrorsR0);
  FreeMem(ErrorsG0);
  FreeMem(ErrorsB0);
  FreeMem(ErrorsR1);
  FreeMem(ErrorsG1);
  FreeMem(ErrorsB1);
  FreeMem(ErrorsR2);
  FreeMem(ErrorsG2);
  FreeMem(ErrorsB2);
  inherited Destroy;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function T5by3Ditherer.Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
var
  ColorR		,
  ColorG		,
  ColorB		: integer; // Error for current pixel
begin
  // Apply red component error correction
  ColorR := Red + (ErrorR0[0] + FDivisor DIV 2) DIV FDivisor;
  if (ColorR < 0) then
    ColorR := 0
  else if (ColorR > 255) then
    ColorR := 255;

  // Apply green component error correction
  ColorG := Green + (ErrorG0[0] + FDivisor DIV 2) DIV FDivisor;
  if (ColorG < 0) then
    ColorG := 0
  else if (ColorG > 255) then
    ColorG := 255;

  // Apply blue component error correction
  ColorB := Blue + (ErrorB0[0] + FDivisor DIV 2) DIV FDivisor;
  if (ColorB < 0) then
    ColorB := 0
  else if (ColorB > 255) then
    ColorB := 255;

  // Map color to palette
  Result := inherited Dither(ColorR, ColorG, ColorB, R, G, B);

  // Propagate red component error
  Propagate(ErrorR0, ErrorR1, ErrorR2, ColorR - R);
  // Propagate green component error
  Propagate(ErrorG0, ErrorG1, ErrorG2, ColorG - G);
  // Propagate blue component error
  Propagate(ErrorB0, ErrorB1, ErrorB2, ColorB - B);

  // Move on to next column
  if (Direction = 1) then
  begin
    inc(longInt(ErrorR0), sizeof(TErrorTerm));
    inc(longInt(ErrorG0), sizeof(TErrorTerm));
    inc(longInt(ErrorB0), sizeof(TErrorTerm));
    inc(longInt(ErrorR1), sizeof(TErrorTerm));
    inc(longInt(ErrorG1), sizeof(TErrorTerm));
    inc(longInt(ErrorB1), sizeof(TErrorTerm));
    inc(longInt(ErrorR2), sizeof(TErrorTerm));
    inc(longInt(ErrorG2), sizeof(TErrorTerm));
    inc(longInt(ErrorB2), sizeof(TErrorTerm));
  end else
  begin
    dec(longInt(ErrorR0), sizeof(TErrorTerm));
    dec(longInt(ErrorG0), sizeof(TErrorTerm));
    dec(longInt(ErrorB0), sizeof(TErrorTerm));
    dec(longInt(ErrorR1), sizeof(TErrorTerm));
    dec(longInt(ErrorG1), sizeof(TErrorTerm));
    dec(longInt(ErrorB1), sizeof(TErrorTerm));
    dec(longInt(ErrorR2), sizeof(TErrorTerm));
    dec(longInt(ErrorG2), sizeof(TErrorTerm));
    dec(longInt(ErrorB2), sizeof(TErrorTerm));
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure T5by3Ditherer.NextLine;
var
  TempErrors		: PErrors;
begin
  FillChar(ErrorsR0^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsG0^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsB0^, sizeof(TErrorTerm)*(Width+4), 0);

  // Swap lines
  TempErrors := ErrorsR0;
  ErrorsR0 := ErrorsR1;
  ErrorsR1 := ErrorsR2;
  ErrorsR2 := TempErrors;

  TempErrors := ErrorsG0;
  ErrorsG0 := ErrorsG1;
  ErrorsG1 := ErrorsG2;
  ErrorsG2 := TempErrors;

  TempErrors := ErrorsB0;
  ErrorsB0 := ErrorsB1;
  ErrorsB1 := ErrorsB2;
  ErrorsB2 := TempErrors;

  inherited NextLine;

  FDirection2 := 2 * Direction;
  if (Direction = 1) then
  begin
    // ErrorsR0[1] gives compiler error, so we
    // use PErrors(longInt(ErrorsR0)+sizeof(TErrorTerm)) instead...
    ErrorR0 := PErrors(longInt(ErrorsR0)+2*sizeof(TErrorTerm));
    ErrorG0 := PErrors(longInt(ErrorsG0)+2*sizeof(TErrorTerm));
    ErrorB0 := PErrors(longInt(ErrorsB0)+2*sizeof(TErrorTerm));
    ErrorR1 := PErrors(longInt(ErrorsR1)+2*sizeof(TErrorTerm));
    ErrorG1 := PErrors(longInt(ErrorsG1)+2*sizeof(TErrorTerm));
    ErrorB1 := PErrors(longInt(ErrorsB1)+2*sizeof(TErrorTerm));
    ErrorR2 := PErrors(longInt(ErrorsR2)+2*sizeof(TErrorTerm));
    ErrorG2 := PErrors(longInt(ErrorsG2)+2*sizeof(TErrorTerm));
    ErrorB2 := PErrors(longInt(ErrorsB2)+2*sizeof(TErrorTerm));
  end else
  begin
    ErrorR0 := @ErrorsR0[Width+1];
    ErrorG0 := @ErrorsG0[Width+1];
    ErrorB0 := @ErrorsB0[Width+1];
    ErrorR1 := @ErrorsR1[Width+1];
    ErrorG1 := @ErrorsG1[Width+1];
    ErrorB1 := @ErrorsB1[Width+1];
    ErrorR2 := @ErrorsR2[Width+1];
    ErrorG2 := @ErrorsG2[Width+1];
    ErrorB2 := @ErrorsB2[Width+1];
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//	TStuckiDitherer
constructor TStuckiDitherer.Create(AWidth: integer; Lookup: TColorLookup);
begin
  inherited Create(AWidth, Lookup);
  FDivisor := 42;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure TStuckiDitherer.Propagate(Errors0, Errors1, Errors2: PErrors; Error: integer);
begin
  if (Error = 0) then
    exit;
  // Propagate Stucki error terms:
  //	...	...	(here)	8/42	4/42
  //	2/42	4/42	8/42	4/42	2/42
  //	1/42	2/42	4/42	2/42	1/42
  inc(Errors2[FDirection2], Error);	// Error * 1
  inc(Errors2[-FDirection2], Error);	// Error * 1

  Error := Error + Error;
  inc(Errors1[FDirection2], Error);	// Error * 2
  inc(Errors1[-FDirection2], Error);	// Error * 2
  inc(Errors2[Direction], Error);	// Error * 2
  inc(Errors2[-Direction], Error);	// Error * 2

  Error := Error + Error;
  inc(Errors0[FDirection2], Error);	// Error * 4
  inc(Errors1[-Direction], Error);	// Error * 4
  inc(Errors1[Direction], Error);	// Error * 4
  inc(Errors2[0], Error);		// Error * 4

  Error := Error + Error;
  inc(Errors0[Direction], Error);	// Error * 8
  inc(Errors1[0], Error);		// Error * 8
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//	TSierraDitherer
constructor TSierraDitherer.Create(AWidth: integer; Lookup: TColorLookup);
begin
  inherited Create(AWidth, Lookup);
  FDivisor := 32;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure TSierraDitherer.Propagate(Errors0, Errors1, Errors2: PErrors; Error: integer);
var
  TempError		: integer;
begin
  if (Error = 0) then
    exit;
  // Propagate Sierra error terms:
  //	...	...	(here)	5/32	3/32
  //	2/32	4/32	5/32	4/32	2/32
  //	...	2/32	3/32	2/32	...
  TempError := Error + Error;
  inc(Errors1[FDirection2], TempError);	// Error * 2
  inc(Errors1[-FDirection2], TempError);// Error * 2
  inc(Errors2[Direction], TempError);	// Error * 2
  inc(Errors2[-Direction], TempError);	// Error * 2

  inc(TempError, Error);
  inc(Errors0[FDirection2], TempError);	// Error * 3
  inc(Errors2[0], TempError);		// Error * 3

  inc(TempError, Error);
  inc(Errors1[-Direction], TempError);	// Error * 4
  inc(Errors1[Direction], TempError);	// Error * 4

  inc(TempError, Error);
  inc(Errors0[Direction], TempError);	// Error * 5
  inc(Errors1[0], TempError);		// Error * 5
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//	TJaJuNiDitherer
constructor TJaJuNiDitherer.Create(AWidth: integer; Lookup: TColorLookup);
begin
  inherited Create(AWidth, Lookup);
  FDivisor := 38;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure TJaJuNiDitherer.Propagate(Errors0, Errors1, Errors2: PErrors; Error: integer);
var
  TempError		: integer;
begin
  if (Error = 0) then
    exit;
  // Propagate Jarvis, Judice and Ninke error terms:
  //	...	...	(here)	8/38	4/38
  //	2/38	4/38	8/38	4/38	2/38
  //	1/38	2/38	4/38	2/38	1/38
  inc(Errors2[FDirection2], Error);	// Error * 1
  inc(Errors2[-FDirection2], Error);	// Error * 1

  TempError := Error + Error;
  inc(Error, TempError);
  inc(Errors1[FDirection2], Error);	// Error * 3
  inc(Errors1[-FDirection2], Error);	// Error * 3
  inc(Errors2[Direction], Error);	// Error * 3
  inc(Errors2[-Direction], Error);	// Error * 3

  inc(Error, TempError);
  inc(Errors0[FDirection2], Error);	// Error * 5
  inc(Errors1[-Direction], Error);	// Error * 5
  inc(Errors1[Direction], Error);	// Error * 5
  inc(Errors2[0], Error);		// Error * 5

  inc(Error, TempError);
  inc(Errors0[Direction], Error);	// Error * 7
  inc(Errors1[0], Error);		// Error * 7
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//	TSteveArcheDitherer
constructor TSteveArcheDitherer.Create(AWidth: integer; Lookup: TColorLookup);
begin
  inherited Create(AWidth, Lookup);

  GetMem(ErrorsR0, sizeof(TErrorTerm)*(Width+6));
  GetMem(ErrorsG0, sizeof(TErrorTerm)*(Width+6));
  GetMem(ErrorsB0, sizeof(TErrorTerm)*(Width+6));
  GetMem(ErrorsR1, sizeof(TErrorTerm)*(Width+6));
  GetMem(ErrorsG1, sizeof(TErrorTerm)*(Width+6));
  GetMem(ErrorsB1, sizeof(TErrorTerm)*(Width+6));
  GetMem(ErrorsR2, sizeof(TErrorTerm)*(Width+6));
  GetMem(ErrorsG2, sizeof(TErrorTerm)*(Width+6));
  GetMem(ErrorsB2, sizeof(TErrorTerm)*(Width+6));
  GetMem(ErrorsR3, sizeof(TErrorTerm)*(Width+6));
  GetMem(ErrorsG3, sizeof(TErrorTerm)*(Width+6));
  GetMem(ErrorsB3, sizeof(TErrorTerm)*(Width+6));
  FillChar(ErrorsR0^, sizeof(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsG0^, sizeof(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsB0^, sizeof(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsR1^, sizeof(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsG1^, sizeof(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsB1^, sizeof(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsR2^, sizeof(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsG2^, sizeof(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsB2^, sizeof(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsR3^, sizeof(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsG3^, sizeof(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsB3^, sizeof(TErrorTerm)*(Width+6), 0);

  FDirection2 := 2 * Direction;
  FDirection3 := 3 * Direction;

  ErrorR0 := PErrors(longInt(ErrorsR0)+3*sizeof(TErrorTerm));
  ErrorG0 := PErrors(longInt(ErrorsG0)+3*sizeof(TErrorTerm));
  ErrorB0 := PErrors(longInt(ErrorsB0)+3*sizeof(TErrorTerm));
  ErrorR1 := PErrors(longInt(ErrorsR1)+3*sizeof(TErrorTerm));
  ErrorG1 := PErrors(longInt(ErrorsG1)+3*sizeof(TErrorTerm));
  ErrorB1 := PErrors(longInt(ErrorsB1)+3*sizeof(TErrorTerm));
  ErrorR2 := PErrors(longInt(ErrorsR2)+3*sizeof(TErrorTerm));
  ErrorG2 := PErrors(longInt(ErrorsG2)+3*sizeof(TErrorTerm));
  ErrorB2 := PErrors(longInt(ErrorsB2)+3*sizeof(TErrorTerm));
  ErrorR3 := PErrors(longInt(ErrorsR3)+3*sizeof(TErrorTerm));
  ErrorG3 := PErrors(longInt(ErrorsG3)+3*sizeof(TErrorTerm));
  ErrorB3 := PErrors(longInt(ErrorsB3)+3*sizeof(TErrorTerm));
end;

destructor TSteveArcheDitherer.Destroy;
begin
  FreeMem(ErrorsR0);
  FreeMem(ErrorsG0);
  FreeMem(ErrorsB0);
  FreeMem(ErrorsR1);
  FreeMem(ErrorsG1);
  FreeMem(ErrorsB1);
  FreeMem(ErrorsR2);
  FreeMem(ErrorsG2);
  FreeMem(ErrorsB2);
  FreeMem(ErrorsR3);
  FreeMem(ErrorsG3);
  FreeMem(ErrorsB3);
  inherited Destroy;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TSteveArcheDitherer.Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
var
  ColorR		,
  ColorG		,
  ColorB		: integer; // Error for current pixel

  // Propagate Stevenson & Arche error terms:
  //	...	...	...	(here)	...	32/200	...
  //    12/200	...	26/200	...	30/200	...	16/200
  //	...	12/200	...	26/200	...	12/200	...
  //	5/200	...	12/200	...	12/200	...	5/200
  procedure Propagate(Errors0, Errors1, Errors2, Errors3: PErrors; Error: integer);
  var
    TempError		: integer;
  begin
    if (Error = 0) then
      exit;
    TempError := 5 * Error;
    inc(Errors3[FDirection3], TempError);	// Error * 5
    inc(Errors3[-FDirection3], TempError);	// Error * 5

    TempError := 12 * Error;
    inc(Errors1[-FDirection3], TempError);	// Error * 12
    inc(Errors2[-FDirection2], TempError);	// Error * 12
    inc(Errors2[FDirection2], TempError);	// Error * 12
    inc(Errors3[-Direction], TempError);	// Error * 12
    inc(Errors3[Direction], TempError);		// Error * 12

    inc(Errors1[FDirection3], 16 * TempError);	// Error * 16

    TempError := 26 * Error;
    inc(Errors1[-Direction], TempError);	// Error * 26
    inc(Errors2[0], TempError);			// Error * 26

    inc(Errors1[Direction], 30 * Error);	// Error * 30

    inc(Errors0[FDirection2], 32 * Error);	// Error * 32
  end;

begin
  // Apply red component error correction
  ColorR := Red + (ErrorR0[0] + 100) DIV 200;
  if (ColorR < 0) then
    ColorR := 0
  else if (ColorR > 255) then
    ColorR := 255;

  // Apply green component error correction
  ColorG := Green + (ErrorG0[0] + 100) DIV 200;
  if (ColorG < 0) then
    ColorG := 0
  else if (ColorG > 255) then
    ColorG := 255;

  // Apply blue component error correction
  ColorB := Blue + (ErrorB0[0] + 100) DIV 200;
  if (ColorB < 0) then
    ColorB := 0
  else if (ColorB > 255) then
    ColorB := 255;

  // Map color to palette
  Result := inherited Dither(ColorR, ColorG, ColorB, R, G, B);

  // Propagate red component error
  Propagate(ErrorR0, ErrorR1, ErrorR2, ErrorR3, ColorR - R);
  // Propagate green component error
  Propagate(ErrorG0, ErrorG1, ErrorG2, ErrorG3, ColorG - G);
  // Propagate blue component error
  Propagate(ErrorB0, ErrorB1, ErrorB2, ErrorB3, ColorB - B);

  // Move on to next column
  if (Direction = 1) then
  begin
    inc(longInt(ErrorR0), sizeof(TErrorTerm));
    inc(longInt(ErrorG0), sizeof(TErrorTerm));
    inc(longInt(ErrorB0), sizeof(TErrorTerm));
    inc(longInt(ErrorR1), sizeof(TErrorTerm));
    inc(longInt(ErrorG1), sizeof(TErrorTerm));
    inc(longInt(ErrorB1), sizeof(TErrorTerm));
    inc(longInt(ErrorR2), sizeof(TErrorTerm));
    inc(longInt(ErrorG2), sizeof(TErrorTerm));
    inc(longInt(ErrorB2), sizeof(TErrorTerm));
    inc(longInt(ErrorR3), sizeof(TErrorTerm));
    inc(longInt(ErrorG3), sizeof(TErrorTerm));
    inc(longInt(ErrorB3), sizeof(TErrorTerm));
  end else
  begin
    dec(longInt(ErrorR0), sizeof(TErrorTerm));
    dec(longInt(ErrorG0), sizeof(TErrorTerm));
    dec(longInt(ErrorB0), sizeof(TErrorTerm));
    dec(longInt(ErrorR1), sizeof(TErrorTerm));
    dec(longInt(ErrorG1), sizeof(TErrorTerm));
    dec(longInt(ErrorB1), sizeof(TErrorTerm));
    dec(longInt(ErrorR2), sizeof(TErrorTerm));
    dec(longInt(ErrorG2), sizeof(TErrorTerm));
    dec(longInt(ErrorB2), sizeof(TErrorTerm));
    dec(longInt(ErrorR3), sizeof(TErrorTerm));
    dec(longInt(ErrorG3), sizeof(TErrorTerm));
    dec(longInt(ErrorB3), sizeof(TErrorTerm));
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure TSteveArcheDitherer.NextLine;
var
  TempErrors		: PErrors;
begin
  FillChar(ErrorsR0^, sizeof(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsG0^, sizeof(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsB0^, sizeof(TErrorTerm)*(Width+6), 0);

  // Swap lines
  TempErrors := ErrorsR0;
  ErrorsR0 := ErrorsR1;
  ErrorsR1 := ErrorsR2;
  ErrorsR2 := ErrorsR3;
  ErrorsR3 := TempErrors;

  TempErrors := ErrorsG0;
  ErrorsG0 := ErrorsG1;
  ErrorsG1 := ErrorsG2;
  ErrorsG2 := ErrorsG3;
  ErrorsG3 := TempErrors;

  TempErrors := ErrorsB0;
  ErrorsB0 := ErrorsB1;
  ErrorsB1 := ErrorsB2;
  ErrorsB2 := ErrorsB3;
  ErrorsB3 := TempErrors;

  inherited NextLine;

  FDirection2 := 2 * Direction;
  FDirection3 := 3 * Direction;

  if (Direction = 1) then
  begin
    // ErrorsR0[1] gives compiler error, so we
    // use PErrors(longInt(ErrorsR0)+sizeof(TErrorTerm)) instead...
    ErrorR0 := PErrors(longInt(ErrorsR0)+3*sizeof(TErrorTerm));
    ErrorG0 := PErrors(longInt(ErrorsG0)+3*sizeof(TErrorTerm));
    ErrorB0 := PErrors(longInt(ErrorsB0)+3*sizeof(TErrorTerm));
    ErrorR1 := PErrors(longInt(ErrorsR1)+3*sizeof(TErrorTerm));
    ErrorG1 := PErrors(longInt(ErrorsG1)+3*sizeof(TErrorTerm));
    ErrorB1 := PErrors(longInt(ErrorsB1)+3*sizeof(TErrorTerm));
    ErrorR2 := PErrors(longInt(ErrorsR2)+3*sizeof(TErrorTerm));
    ErrorG2 := PErrors(longInt(ErrorsG2)+3*sizeof(TErrorTerm));
    ErrorB2 := PErrors(longInt(ErrorsB2)+3*sizeof(TErrorTerm));
    ErrorR3 := PErrors(longInt(ErrorsR3)+3*sizeof(TErrorTerm));
    ErrorG3 := PErrors(longInt(ErrorsG3)+3*sizeof(TErrorTerm));
    ErrorB3 := PErrors(longInt(ErrorsB3)+3*sizeof(TErrorTerm));
  end else
  begin
    ErrorR0 := @ErrorsR0[Width+2];
    ErrorG0 := @ErrorsG0[Width+2];
    ErrorB0 := @ErrorsB0[Width+2];
    ErrorR1 := @ErrorsR1[Width+2];
    ErrorG1 := @ErrorsG1[Width+2];
    ErrorB1 := @ErrorsB1[Width+2];
    ErrorR2 := @ErrorsR2[Width+2];
    ErrorG2 := @ErrorsG2[Width+2];
    ErrorB2 := @ErrorsB2[Width+2];
    ErrorR3 := @ErrorsR2[Width+2];
    ErrorG3 := @ErrorsG2[Width+2];
    ErrorB3 := @ErrorsB2[Width+2];
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//	TBurkesDitherer
constructor TBurkesDitherer.Create(AWidth: integer; Lookup: TColorLookup);
begin
  inherited Create(AWidth, Lookup);

  GetMem(ErrorsR0, sizeof(TErrorTerm)*(Width+4));
  GetMem(ErrorsG0, sizeof(TErrorTerm)*(Width+4));
  GetMem(ErrorsB0, sizeof(TErrorTerm)*(Width+4));
  GetMem(ErrorsR1, sizeof(TErrorTerm)*(Width+4));
  GetMem(ErrorsG1, sizeof(TErrorTerm)*(Width+4));
  GetMem(ErrorsB1, sizeof(TErrorTerm)*(Width+4));
  FillChar(ErrorsR0^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsG0^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsB0^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsR1^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsG1^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsB1^, sizeof(TErrorTerm)*(Width+4), 0);

  FDirection2 := 2 * Direction;
  ErrorR0 := PErrors(longInt(ErrorsR0)+2*sizeof(TErrorTerm));
  ErrorG0 := PErrors(longInt(ErrorsG0)+2*sizeof(TErrorTerm));
  ErrorB0 := PErrors(longInt(ErrorsB0)+2*sizeof(TErrorTerm));
  ErrorR1 := PErrors(longInt(ErrorsR1)+2*sizeof(TErrorTerm));
  ErrorG1 := PErrors(longInt(ErrorsG1)+2*sizeof(TErrorTerm));
  ErrorB1 := PErrors(longInt(ErrorsB1)+2*sizeof(TErrorTerm));
end;

destructor TBurkesDitherer.Destroy;
begin
  FreeMem(ErrorsR0);
  FreeMem(ErrorsG0);
  FreeMem(ErrorsB0);
  FreeMem(ErrorsR1);
  FreeMem(ErrorsG1);
  FreeMem(ErrorsB1);
  inherited Destroy;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TBurkesDitherer.Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): char;
var
  ErrorR		,
  ErrorG		,
  ErrorB		: integer; // Error for current pixel

  // Propagate Burkes error terms:
  //	...	...	(here)	8/32	4/32
  //	2/32	4/32	8/32	4/32	2/32
  procedure Propagate(Errors0, Errors1: PErrors; Error: integer);
  begin
    if (Error = 0) then
      exit;
    inc(Error, Error);
    inc(Errors1[FDirection2], Error);	// Error * 2
    inc(Errors1[-FDirection2], Error);	// Error * 2

    inc(Error, Error);
    inc(Errors0[FDirection2], Error);	// Error * 4
    inc(Errors1[-Direction], Error);	// Error * 4
    inc(Errors1[Direction], Error);	// Error * 4

    inc(Error, Error);
    inc(Errors0[Direction], Error);	// Error * 8
    inc(Errors1[0], Error);		// Error * 8
  end;

begin
  // Apply red component error correction
  ErrorR := Red + (ErrorR0[0] + 16) DIV 32;
  if (ErrorR < 0) then
    ErrorR := 0
  else if (ErrorR > 255) then
    ErrorR := 255;

  // Apply green component error correction
  ErrorG := Green + (ErrorG0[0] + 16) DIV 32;
  if (ErrorG < 0) then
    ErrorG := 0
  else if (ErrorG > 255) then
    ErrorG := 255;

  // Apply blue component error correction
  ErrorB := Blue + (ErrorB0[0] + 16) DIV 32;
  if (ErrorB < 0) then
    ErrorB := 0
  else if (ErrorB > 255) then
    ErrorB := 255;

  // Map color to palette
  Result := inherited Dither(ErrorR, ErrorG, ErrorB, R, G, B);

  // Propagate red component error
  Propagate(ErrorR0, ErrorR1, ErrorR - R);
  // Propagate green component error
  Propagate(ErrorG0, ErrorG1, ErrorG - G);
  // Propagate blue component error
  Propagate(ErrorB0, ErrorB1, ErrorB - B);

  // Move on to next column
  if (Direction = 1) then
  begin
    inc(longInt(ErrorR0), sizeof(TErrorTerm));
    inc(longInt(ErrorG0), sizeof(TErrorTerm));
    inc(longInt(ErrorB0), sizeof(TErrorTerm));
    inc(longInt(ErrorR1), sizeof(TErrorTerm));
    inc(longInt(ErrorG1), sizeof(TErrorTerm));
    inc(longInt(ErrorB1), sizeof(TErrorTerm));
  end else
  begin
    dec(longInt(ErrorR0), sizeof(TErrorTerm));
    dec(longInt(ErrorG0), sizeof(TErrorTerm));
    dec(longInt(ErrorB0), sizeof(TErrorTerm));
    dec(longInt(ErrorR1), sizeof(TErrorTerm));
    dec(longInt(ErrorG1), sizeof(TErrorTerm));
    dec(longInt(ErrorB1), sizeof(TErrorTerm));
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure TBurkesDitherer.NextLine;
var
  TempErrors		: PErrors;
begin
  FillChar(ErrorsR0^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsG0^, sizeof(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsB0^, sizeof(TErrorTerm)*(Width+4), 0);

  // Swap lines
  TempErrors := ErrorsR0;
  ErrorsR0 := ErrorsR1;
  ErrorsR1 := TempErrors;

  TempErrors := ErrorsG0;
  ErrorsG0 := ErrorsG1;
  ErrorsG1 := TempErrors;

  TempErrors := ErrorsB0;
  ErrorsB0 := ErrorsB1;
  ErrorsB1 := TempErrors;

  inherited NextLine;

  FDirection2 := 2 * Direction;
  if (Direction = 1) then
  begin
    // ErrorsR0[1] gives compiler error, so we
    // use PErrors(longInt(ErrorsR0)+sizeof(TErrorTerm)) instead...
    ErrorR0 := PErrors(longInt(ErrorsR0)+2*sizeof(TErrorTerm));
    ErrorG0 := PErrors(longInt(ErrorsG0)+2*sizeof(TErrorTerm));
    ErrorB0 := PErrors(longInt(ErrorsB0)+2*sizeof(TErrorTerm));
    ErrorR1 := PErrors(longInt(ErrorsR1)+2*sizeof(TErrorTerm));
    ErrorG1 := PErrors(longInt(ErrorsG1)+2*sizeof(TErrorTerm));
    ErrorB1 := PErrors(longInt(ErrorsB1)+2*sizeof(TErrorTerm));
  end else
  begin
    ErrorR0 := @ErrorsR0[Width+1];
    ErrorG0 := @ErrorsG0[Width+1];
    ErrorB0 := @ErrorsB0[Width+1];
    ErrorR1 := @ErrorsR1[Width+1];
    ErrorG1 := @ErrorsG1[Width+1];
    ErrorB1 := @ErrorsB1[Width+1];
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
//			Octree Color Quantization Engine
//
////////////////////////////////////////////////////////////////////////////////
//  Adapted from Earl F. Glynn's ColorQuantizationLibrary, March 1998
////////////////////////////////////////////////////////////////////////////////
type
  TOctreeNode = class;	// Forward definition so TReducibleNodes can be declared

  TReducibleNodes = array[0..7] of TOctreeNode;

  TOctreeNode = Class(TObject)
  public
    IsLeaf		: Boolean;
    PixelCount		: integer;
    RedSum		: integer;
    GreenSum		: integer;
    BlueSum		: integer;
    Next		: TOctreeNode;
    Child		: TReducibleNodes;

    constructor Create(Level: integer; ColorBits: integer; var LeafCount: integer;
      var ReducibleNodes: TReducibleNodes);
    destructor Destroy; override;
  end;

  TColorQuantizer = class(TObject)
  private
    FTree		: TOctreeNode;
    FLeafCount		: integer;
    FReducibleNodes	: TReducibleNodes;
    FMaxColors		: integer;
    FColorBits		: integer;

  protected
    procedure AddColor(var Node: TOctreeNode; r, g, b: byte; ColorBits: integer;
      Level: integer; var LeafCount: integer; var ReducibleNodes: TReducibleNodes);
    procedure DeleteTree(var Node: TOctreeNode);
    procedure GetPaletteColors(const Node: TOctreeNode;
      var RGBQuadArray: TRGBQuadArray; var Index: integer);
    procedure ReduceTree(ColorBits: integer; var LeafCount: integer;
      var ReducibleNodes: TReducibleNodes);

  public
    constructor Create(MaxColors: integer; ColorBits: integer);
    destructor Destroy; override;

    procedure GetColorTable(var RGBQuadArray: TRGBQuadArray);
    function ProcessImage(const DIB: TDIBReader): boolean;

    property ColorCount: integer read FLeafCount;
  end;

constructor TOctreeNode.Create(Level: integer; ColorBits: integer;
  var LeafCount: integer; var ReducibleNodes: TReducibleNodes);
var
  i			: integer;
begin
  PixelCount := 0;
  RedSum := 0;
  GreenSum := 0;
  BlueSum := 0;
  for i := Low(Child) to High(Child) do
    Child[i] := nil;

  IsLeaf := (Level = ColorBits);
  if (IsLeaf) then
  begin
    Next := nil;
    inc(LeafCount);
  end else
  begin
    Next := ReducibleNodes[Level];
    ReducibleNodes[Level] := self;
  end;
end;

destructor TOctreeNode.Destroy;
var
  i			: integer;
begin
  for i := High(Child) downto Low(Child) do
    Child[i].Free;
end;

constructor TColorQuantizer.Create(MaxColors: integer; ColorBits: integer);
var
  i			: integer;
begin
  ASSERT(ColorBits <= 8, 'ColorBits must be 8 or less');

  FTree := nil;
  FLeafCount := 0;

  // Initialize all nodes even though only ColorBits+1 of them are needed
  for i := Low(FReducibleNodes) to High(FReducibleNodes) do
    FReducibleNodes[i] := nil;

  FMaxColors := MaxColors;
  FColorBits := ColorBits;
end;

destructor TColorQuantizer.Destroy;
begin
  if (FTree <> nil) then
    DeleteTree(FTree);
end;

procedure TColorQuantizer.GetColorTable(var RGBQuadArray: TRGBQuadArray);
var
  Index			: integer;
begin
  Index := 0;
  GetPaletteColors(FTree, RGBQuadArray, Index);
end;

// Handles passed to ProcessImage should refer to DIB sections, not DDBs.
// In certain cases, specifically when it's called upon to process 1, 4, or
// 8-bit per pixel images on systems with palettized display adapters,
// ProcessImage can produce incorrect results if it's passed a handle to a
// DDB.
function TColorQuantizer.ProcessImage(const DIB: TDIBReader): boolean;
var
  i			,
  j			: integer;
  ScanLine		: pointer;
  Pixel			: PRGBTriple;
begin
  Result := True;

  for j := 0 to DIB.Bitmap.Height-1 do
  begin
    Scanline := DIB.Scanline[j];
    Pixel := ScanLine;
    for i := 0 to DIB.Bitmap.Width-1 do
    begin
      with Pixel^ do
        AddColor(FTree, rgbtRed, rgbtGreen, rgbtBlue,
                 FColorBits, 0, FLeafCount, FReducibleNodes);

      while FLeafCount > FMaxColors do
        ReduceTree(FColorbits, FLeafCount, FReducibleNodes);
      inc(Pixel);
    end;
  end;
end;

procedure TColorQuantizer.AddColor(var Node: TOctreeNode; r,g,b: byte;
  ColorBits: integer; Level: integer; var LeafCount: integer;
  var ReducibleNodes: TReducibleNodes);
const
  Mask:  array[0..7] of BYTE = ($80, $40, $20, $10, $08, $04, $02, $01);
var
  Index			: integer;
  Shift			: integer;
begin
  // If the node doesn't exist, create it.
  if (Node = nil) then
    Node := TOctreeNode.Create(Level, ColorBits, LeafCount, ReducibleNodes);

  if (Node.IsLeaf) then
  begin
    inc(Node.PixelCount);
    inc(Node.RedSum, r);
    inc(Node.GreenSum, g);
    inc(Node.BlueSum, b);
  end else
  begin
    // Recurse a level deeper if the node is not a leaf.
    Shift := 7 - Level;

    Index := (((r and mask[Level]) SHR Shift) SHL 2)  or
             (((g and mask[Level]) SHR Shift) SHL 1)  or
              ((b and mask[Level]) SHR Shift);
    AddColor(Node.Child[Index], r, g, b, ColorBits, Level+1, LeafCount, ReducibleNodes);
  end;
end;

procedure TColorQuantizer.DeleteTree(var Node: TOctreeNode);
var
  i			: integer;
begin
  for i := High(TReducibleNodes) downto Low(TReducibleNodes) do
    if (Node.Child[i] <> nil) then
      DeleteTree(Node.Child[i]);

  Node.Free;
  Node := nil;
end;

procedure TColorQuantizer.GetPaletteColors(const Node: TOctreeNode;
  var RGBQuadArray: TRGBQuadArray; var Index: integer);
var
  i			: integer;
begin
  if (Node.IsLeaf) then
  begin
    with RGBQuadArray[Index] do
    begin
      if (Node.PixelCount <> 0) then
      begin
        rgbRed   := BYTE(Node.RedSum   DIV Node.PixelCount);
        rgbGreen := BYTE(Node.GreenSum DIV Node.PixelCount);
        rgbBlue  := BYTE(Node.BlueSum  DIV Node.PixelCount);
      end else
      begin
        rgbRed := 0;
        rgbGreen := 0;
        rgbBlue := 0;
      end;
      rgbReserved := 0;
    end;
    inc(Index);
  end else
  begin
    for i := Low(Node.Child) to High(Node.Child) do
      if (Node.Child[i] <> nil) then
        GetPaletteColors(Node.Child[i], RGBQuadArray, Index);
  end;
end;

procedure TColorQuantizer.ReduceTree(ColorBits: integer; var LeafCount: integer;
  var ReducibleNodes: TReducibleNodes);
var
  RedSum		,
  GreenSum		,
  BlueSum 		: integer;
  Children		: integer;
  i			: integer;
  Node			: TOctreeNode;
begin
  // Find the deepest level containing at least one reducible node
  i := Colorbits - 1;
  while (i > 0) and (ReducibleNodes[i] = nil) do
    dec(i);

  // Reduce the node most recently added to the list at level i.
  Node := ReducibleNodes[i];
  ReducibleNodes[i] := Node.Next;

  RedSum   := 0;
  GreenSum := 0;
  BlueSum  := 0;
  Children := 0;

  for i := Low(ReducibleNodes) to High(ReducibleNodes) do
    if (Node.Child[i] <> nil) then
    begin
      inc(RedSum, Node.Child[i].RedSum);
      inc(GreenSum, Node.Child[i].GreenSum);
      inc(BlueSum, Node.Child[i].BlueSum);
      inc(Node.PixelCount, Node.Child[i].PixelCount);
      Node.Child[i].Free;
      Node.Child[i] := nil;
      inc(Children);
    end;

  Node.IsLeaf := TRUE;
  Node.RedSum := RedSum;
  Node.GreenSum := GreenSum;
  Node.BlueSum := BlueSum;
  dec(LeafCount, Children-1);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Octree Color Quantization Wrapper
//
////////////////////////////////////////////////////////////////////////////////
//	Adapted from Earl F. Glynn's PaletteLibrary, March 1998
////////////////////////////////////////////////////////////////////////////////

// Wrapper for internal use - uses TDIBReader for bitmap access
function doCreateOptimizedPaletteFromSingleBitmap(const DIB: TDIBReader;
  Colors, ColorBits: integer; Windows: boolean): hPalette;
var
  SystemPalette		: HPalette;
  ColorQuantizer	: TColorQuantizer;
  i			: integer;
  LogicalPalette	: TMaxLogPalette;
  RGBQuadArray		: TRGBQuadArray;
  Offset		: integer;
begin
  LogicalPalette.palVersion := $0300;
  LogicalPalette.palNumEntries := Colors;

  if (Windows) then
  begin
    // Get the windows 20 color system palette
    SystemPalette := GetStockObject(DEFAULT_PALETTE);
    GetPaletteEntries(SystemPalette, 0, 10, LogicalPalette.palPalEntry[0]);
    GetPaletteEntries(SystemPalette, 10, 10, LogicalPalette.palPalEntry[245]);
    Colors := 236;
    Offset := 10;
    LogicalPalette.palNumEntries := 256;
  end else
    Offset := 0;

  // Normally for 24-bit images, use ColorBits of 5 or 6.  For 8-bit images
  // use ColorBits = 8.
  ColorQuantizer := TColorQuantizer.Create(Colors, ColorBits);
  try
    ColorQuantizer.ProcessImage(DIB);
    ColorQuantizer.GetColorTable(RGBQuadArray);
  finally
    ColorQuantizer.Free;
  end;

  for i := 0 to Colors-1 do
    with LogicalPalette.palPalEntry[i+Offset] do
    begin
      peRed   := RGBQuadArray[i].rgbRed;
      peGreen := RGBQuadArray[i].rgbGreen;
      peBlue  := RGBQuadArray[i].rgbBlue;
      peFlags := RGBQuadArray[i].rgbReserved;
    end;
  Result := CreatePalette(pLogPalette(@LogicalPalette)^);
end;

function CreateOptimizedPaletteFromSingleBitmap(const Bitmap: TBitmap;
  Colors, ColorBits: integer; Windows: boolean): hPalette;
var
  DIB			: TDIBReader;
begin
  DIB := TDIBReader.Create(Bitmap, pf24bit);
  try
    Result := doCreateOptimizedPaletteFromSingleBitmap(DIB, Colors, ColorBits, Windows);
  finally
    DIB.Free;
  end;
end;

function CreateOptimizedPaletteFromManyBitmaps(Bitmaps: TList; Colors, ColorBits: integer;
  Windows: boolean): hPalette;
var
  SystemPalette		: HPalette;
  ColorQuantizer	: TColorQuantizer;
  i			: integer;
  LogicalPalette	: TMaxLogPalette;
  RGBQuadArray		: TRGBQuadArray;
  Offset		: integer;
  DIB			: TDIBReader;
begin
  if (Bitmaps = nil) or (Bitmaps.Count = 0) then
    Error(sInvalidBitmapList);

  LogicalPalette.palVersion := $0300;
  LogicalPalette.palNumEntries := Colors;

  if (Windows) then
  begin
    // Get the windows 20 color system palette
    SystemPalette := GetStockObject(DEFAULT_PALETTE);
    GetPaletteEntries(SystemPalette, 0, 10, LogicalPalette.palPalEntry[0]);
    GetPaletteEntries(SystemPalette, 10, 10, LogicalPalette.palPalEntry[245]);
    Colors := 236;
    Offset := 10;
    LogicalPalette.palNumEntries := 256;
  end else
    Offset := 0;

  // Normally for 24-bit images, use ColorBits of 5 or 6.  For 8-bit images
  // use ColorBits = 8.
  ColorQuantizer := TColorQuantizer.Create(Colors, ColorBits);
  try
    for i := 0 to Bitmaps.Count-1 do
    begin
      DIB := TDIBReader.Create(TBitmap(Bitmaps[i]), pf24bit);
      try
        ColorQuantizer.ProcessImage(DIB);
      finally
        DIB.Free;
      end;
    end;
    ColorQuantizer.GetColorTable(RGBQuadArray);
  finally
    ColorQuantizer.Free;
  end;

  for i := 0 to Colors-1 do
    with LogicalPalette.palPalEntry[i+Offset] do
    begin
      peRed   := RGBQuadArray[i].rgbRed;
      peGreen := RGBQuadArray[i].rgbGreen;
      peBlue  := RGBQuadArray[i].rgbBlue;
      peFlags := RGBQuadArray[i].rgbReserved;
    end;
  Result := CreatePalette(pLogPalette(@LogicalPalette)^);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Color reduction
//
////////////////////////////////////////////////////////////////////////////////
{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
//: Reduces the color depth of a bitmap using color quantization and dithering.
function ReduceColors(Bitmap: TBitmap; ColorReduction: TColorReduction;
  DitherMode: TDitherMode; ReductionBits: integer; CustomPalette: hPalette): TBitmap;
var
  Palette		: hPalette;
  ColorLookup		: TColorLookup;
  Ditherer		: TDitherEngine;
  Row			: Integer;
  DIBResult		: TDIBWriter;
  DIBSource		: TDIBReader;
  SrcScanLine		,
  Src			: PRGBTriple;
  DstScanLine		,
  Dst			: PChar;
  BGR			: TRGBTriple;
{$ifdef DEBUG_DITHERPERFORMANCE}
  TimeStart		,
  TimeStop		: DWORD;
{$endif}

  function GrayScalePalette: hPalette;
  var
    i			: integer;
    Pal			: TMaxLogPalette;
  begin
    Pal.palVersion := $0300;
    Pal.palNumEntries := 256;
    for i := 0 to 255 do
    begin
      with (Pal.palPalEntry[i]) do
      begin
        peRed := i;
        peGreen := i;
        peBlue  := i;
        peFlags := PC_NOCOLLAPSE;
      end;
    end;
    Result := CreatePalette(pLogPalette(@Pal)^);
  end;

  function MonochromePalette: hPalette;
  var
    i			: integer;
    Pal			: TMaxLogPalette;
  const
    Values		: array[0..1] of byte
    			= (0, 255);
  begin
    Pal.palVersion := $0300;
    Pal.palNumEntries := 2;
    for i := 0 to 1 do
    begin
      with (Pal.palPalEntry[i]) do
      begin
        peRed := Values[i];
        peGreen := Values[i];
        peBlue  := Values[i];
        peFlags := PC_NOCOLLAPSE;
      end;
    end;
    Result := CreatePalette(pLogPalette(@Pal)^);
  end;

  function WindowsGrayScalePalette: hPalette;
  var
    i			: integer;
    Pal			: TMaxLogPalette;
  const
    Values		: array[0..3] of byte
    			= (0, 128, 192, 255);
  begin
    Pal.palVersion := $0300;
    Pal.palNumEntries := 4;
    for i := 0 to 3 do
    begin
      with (Pal.palPalEntry[i]) do
      begin
        peRed := Values[i];
        peGreen := Values[i];
        peBlue  := Values[i];
        peFlags := PC_NOCOLLAPSE;
      end;
    end;
    Result := CreatePalette(pLogPalette(@Pal)^);
  end;

  function WindowsHalftonePalette: hPalette;
  var
    DC			: HDC;
  begin
    DC := GDICheck(GetDC(0));
    try
      Result := CreateHalfTonePalette(DC);
    finally
      ReleaseDC(0, DC);
    end;
  end;

begin
{$ifdef DEBUG_DITHERPERFORMANCE}
  timeBeginPeriod(5);
  TimeStart := timeGetTime;
{$endif}

  Result := TBitmap.Create;
  try

    if (ColorReduction = rmNone) then
    begin
      Result.Assign(Bitmap);
{$ifndef VER9x}
      SetPixelFormat(Result, pf24bit);
{$endif}
      exit;
    end;

{$IFNDEF VER9x}
    if (Bitmap.Width*Bitmap.Height > BitmapAllocationThreshold) then
      SetPixelFormat(Result, pf1bit); // To reduce resource consumption of resize
{$ENDIF}

    ColorLookup := nil;
    Ditherer := nil;
    DIBResult := nil;
    DIBSource := nil;
    Palette := 0;
    try // Protect above resources

      // Dithering and color mapper only supports 24 bit bitmaps,
      // so we have convert the source bitmap to the appropiate format.
      DIBSource := TDIBReader.Create(Bitmap, pf24bit);

      // Create a palette based on current options
      case (ColorReduction) of
        rmQuantize:
          Palette := doCreateOptimizedPaletteFromSingleBitmap(DIBSource, 1 SHL ReductionBits, 8, False);
        rmQuantizeWindows:
          Palette := CreateOptimizedPaletteFromSingleBitmap(Bitmap, 256, 8, True);
        rmNetscape:
          Palette := WebPalette;
        rmGrayScale:
          Palette := GrayScalePalette;
        rmMonochrome:
          Palette := MonochromePalette;
        rmWindowsGray:
          Palette := WindowsGrayScalePalette;
        rmWindows20:
          Palette := GetStockObject(DEFAULT_PALETTE);
        rmWindows256:
          Palette := WindowsHalftonePalette;
        rmPalette:
          Palette := CopyPalette(CustomPalette);
      else
        exit;
      end;

      { TODO -oanme -cImprovement : Gray scale conversion should be done prior to dithering/mapping. Otherwise corrected values will be converted multiple times. }

      // Create a color mapper based on current options
      case (ColorReduction) of
        // For some strange reason my fast and dirty color lookup
        // is more precise that Windows GetNearestPaletteIndex...
        // rmWindows20:
        //  ColorLookup := TSlowColorLookup.Create(Palette);
        // rmWindowsGray:
        //  ColorLookup := TGrayWindowsLookup.Create(Palette);
        rmQuantize:
          ColorLookup := TFastColorLookup.Create(Palette);
        rmNetscape:
          ColorLookup := TNetscapeColorLookup.Create(Palette);
        rmGrayScale:
          ColorLookup := TGrayScaleLookup.Create(Palette);
        rmMonochrome:
          ColorLookup := TMonochromeLookup.Create(Palette);
      else
        ColorLookup := TFastColorLookup.Create(Palette);
      end;

      // Nothing to do if palette doesn't contain any colors
      if (ColorLookup.Colors = 0) then
        exit;

      // Create a ditherer based on current options
      case (DitherMode) of
        dmNearest:
          Ditherer := TDitherEngine.Create(Bitmap.Width, ColorLookup);
        dmFloydSteinberg:
          Ditherer := TFloydSteinbergDitherer.Create(Bitmap.Width, ColorLookup);
        dmStucki:
          Ditherer := TStuckiDitherer.Create(Bitmap.Width, ColorLookup);
        dmSierra:
          Ditherer := TSierraDitherer.Create(Bitmap.Width, ColorLookup);
        dmJaJuNI:
          Ditherer := TJaJuNIDitherer.Create(Bitmap.Width, ColorLookup);
        dmSteveArche:
          Ditherer := TSteveArcheDitherer.Create(Bitmap.Width, ColorLookup);
        dmBurkes:
          Ditherer := TBurkesDitherer.Create(Bitmap.Width, ColorLookup);
      else
        exit;
      end;

      // The processed bitmap is returned in pf8bit format
      DIBResult := TDIBWriter.Create(Result, pf8bit, Bitmap.Width, Bitmap.Height,
        Palette);

      // Process the image
      Row := 0;
      while (Row < Bitmap.Height) do
      begin
        SrcScanline := DIBSource.ScanLine[Row];
        DstScanline := DIBResult.ScanLine[Row];
        Src := pointer(longInt(SrcScanLine) + Ditherer.Column*sizeof(TRGBTriple));
        Dst := pointer(longInt(DstScanLine) + Ditherer.Column);

        while (Ditherer.Column < Ditherer.Width) and (Ditherer.Column >= 0) do
        begin
          BGR := Src^;
          // Dither and map a single pixel
          Dst^ := Ditherer.Dither(BGR.rgbtRed, BGR.rgbtGreen, BGR.rgbtBlue,
            BGR.rgbtRed, BGR.rgbtGreen, BGR.rgbtBlue);

          inc(Src, Ditherer.Direction);
          inc(Dst, Ditherer.Direction);
        end;

        Inc(Row);
        Ditherer.NextLine;
      end;
    finally
      if (ColorLookup <> nil) then
        ColorLookup.Free;
      if (Ditherer <> nil) then
        Ditherer.Free;
      if (DIBResult <> nil) then
        DIBResult.Free;
      if (DIBSource <> nil) then
        DIBSource.Free;
      // Must delete palette after TDIBWriter since TDIBWriter uses palette 
      if (Palette <> 0) then
        DeleteObject(Palette);
    end;
  except
    Result.Free;
    raise;
  end;

{$ifdef DEBUG_DITHERPERFORMANCE}
  TimeStop := timeGetTime;
  ShowMessage(format('Dithered %d pixels in %d mS, Rate %d pixels/mS (%d pixels/S)',
    [Bitmap.Height*Bitmap.Width, TimeStop-TimeStart,
    MulDiv(Bitmap.Height, Bitmap.Width, TimeStop-TimeStart+1),
    MulDiv(Bitmap.Height, Bitmap.Width * 1000, TimeStop-TimeStart+1)]));
  timeEndPeriod(5);
{$endif}
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFColorMap
//
////////////////////////////////////////////////////////////////////////////////
const
  InitColorMapSize = 16;
  DeltaColorMapSize = 32;

//: Creates an instance of a TGIFColorMap object.
constructor TGIFColorMap.Create;
begin
  inherited Create;
  FColorMap := nil;
  FCapacity := 0;
  FCount := 0;
  FOptimized := False;
end;

//: Destroys an instance of a TGIFColorMap object.
destructor TGIFColorMap.Destroy;
begin
  Clear;
  Changed;
  inherited Destroy;
end;

//: Empties the color map.
procedure TGIFColorMap.Clear;
begin
  if (FColorMap <> nil) then
    FreeMem(FColorMap);
  FColorMap := nil;
  FCapacity := 0;
  FCount := 0;
  FOptimized := False;
end;

//: Converts a Windows color value to a RGB value.
class function TGIFColorMap.Color2RGB(Color: TColor): TGIFColor;
begin
  Result.Blue := (Color shr 16) and $FF;
  Result.Green := (Color shr 8) and $FF;
  Result.Red  := Color and $FF;
end;

//: Converts a RGB value to a Windows color value.
class function TGIFColorMap.RGB2Color(Color: TGIFColor): TColor;
begin
  Result := (Color.Blue SHL 16) OR (Color.Green SHL 8) OR Color.Red;
end;

//: Saves the color map to a stream.
procedure TGIFColorMap.SaveToStream(Stream: TStream);
var
  Dummies		: integer;
  Dummy			: TGIFColor;
begin
  if (FCount = 0) then
    exit;
  Stream.WriteBuffer(FColorMap^, FCount*sizeof(TGIFColor));
  Dummies := (1 SHL BitsPerPixel)-FCount;
  Dummy.Red := 0;
  Dummy.Green := 0;
  Dummy.Blue := 0;
  while (Dummies > 0) do
  begin
    Stream.WriteBuffer(Dummy, sizeof(TGIFColor));
    dec(Dummies);
  end;
end;

//: Loads the color map from a stream.
procedure TGIFColorMap.LoadFromStream(Stream: TStream; Count: integer);
begin
  Clear;
  SetCapacity(Count);
  ReadCheck(Stream, FColorMap^, Count*sizeof(TGIFColor));
  FCount := Count;
end;

//: Returns the position of a color in the color map.
function TGIFColorMap.IndexOf(Color: TColor): integer;
var
  RGB			: TGIFColor;
begin
  RGB := Color2RGB(Color);
  if (FOptimized) then
  begin
    // Optimized palette has most frequently occuring entries first
    Result := 0;
    // Reverse search to (hopefully) check latest colors first
    while (Result < FCount) do
      with (FColorMap^[Result]) do
      begin
        if (RGB.Red = Red) and (RGB.Green = Green) and (RGB.Blue = Blue) then
          exit;
        Inc(Result);
      end;
    Result := -1;
  end else
  begin
    Result := FCount-1;
    // Reverse search to (hopefully) check latest colors first
    while (Result >= 0) do
      with (FColorMap^[Result]) do
      begin
        if (RGB.Red = Red) and (RGB.Green = Green) and (RGB.Blue = Blue) then
          exit;
        Dec(Result);
      end;
  end;
end;

procedure TGIFColorMap.SetCapacity(Size: integer);
begin
  if (Size >= FCapacity) then
  begin
    if (Size <= InitColorMapSize) then
      FCapacity := InitColorMapSize
    else
      FCapacity := (Size + DeltaColorMapSize - 1) DIV DeltaColorMapSize * DeltaColorMapSize;
    if (FCapacity > GIFMaxColors) then
      FCapacity := GIFMaxColors;
    ReallocMem(FColorMap, FCapacity * sizeof(TGIFColor));
  end;
end;

//: Imports a Windows palette into the color map.
procedure TGIFColorMap.ImportPalette(Palette: HPalette);
type
  PalArray =  array[byte] of TPaletteEntry;
var
  Pal			: PalArray;
  NewCount		: integer;
  i			: integer;
begin
  Clear;
  NewCount := GetPaletteEntries(Palette, 0, 256, pal);
  if (NewCount = 0) then
    exit;
  SetCapacity(NewCount);
  for i := 0 to NewCount-1 do
    with FColorMap[i], Pal[i] do
    begin
      Red := peRed;
      Green := peGreen;
      Blue := peBlue;
    end;
  FCount := NewCount;
  Changed;
end;

//: Imports a color map structure into the color map.
procedure TGIFColorMap.ImportColorMap(Map: TColorMap; Count: integer);
begin
  Clear;
  if (Count = 0) then
    exit;
  SetCapacity(Count);
  FCount := Count;

  System.Move(Map, FColorMap^, FCount * sizeof(TGIFColor));

  Changed;
end;

//: Imports a Windows palette structure into the color map.
procedure TGIFColorMap.ImportColorTable(Pal: pointer; Count: integer);
var
  i			: integer;
begin
  Clear;
  if (Count = 0) then
    exit;
  SetCapacity(Count);
  for i := 0 to Count-1 do
    with FColorMap[i], PRGBQuadArray(Pal)[i] do
    begin
      Red := rgbRed;
      Green := rgbGreen;
      Blue := rgbBlue;
    end;
  FCount := Count;
  Changed;
end;

//: Imports the color table of a DIB into the color map.
procedure TGIFColorMap.ImportDIBColors(Handle: HDC);
var
  Pal			: Pointer;
  NewCount		: integer;
begin
  Clear;
  GetMem(Pal, sizeof(TRGBQuad) * 256);
  try
    NewCount := GetDIBColorTable(Handle, 0, 256, Pal^);
    ImportColorTable(Pal, NewCount);
  finally
    FreeMem(Pal);
  end;
  Changed;
end;

//: Creates a Windows palette from the color map.
function TGIFColorMap.ExportPalette: HPalette;
var
  Pal			: TMaxLogPalette;
  i			: Integer;
begin
  if (Count = 0) then
  begin
    Result := 0;
    exit;
  end;
  Pal.palVersion := $300;
  Pal.palNumEntries := Count;
  for i := 0 to Count-1 do
    with FColorMap[i], Pal.palPalEntry[i] do
    begin
      peRed := Red;
      peGreen := Green;
      peBlue := Blue;
      peFlags := PC_NOCOLLAPSE; { TODO -oanme -cImprovement : Verify that PC_NOCOLLAPSE is the correct value to use. }
    end;
  Result := CreatePalette(PLogPalette(@Pal)^);
end;

//: Adds a color to the color map.
function TGIFColorMap.Add(Color: TColor): integer;
begin
  if (FCount >= GIFMaxColors) then
    // Color map full
    Error(sTooManyColors);

  Result := FCount;
  if (Result >= FCapacity) then
    SetCapacity(FCount+1);
  FColorMap^[FCount] := Color2RGB(Color);
  inc(FCount);
  FOptimized := False;
  Changed;
end;

function TGIFColorMap.AddUnique(Color: TColor): integer;
begin
  // Look up color before add (same as IndexOf)
  Result := IndexOf(Color);
  if (Result >= 0) then
    // Color already in map
    exit;

  Result := Add(Color);
end;

//: Removes a color from the color map.
procedure TGIFColorMap.Delete(Index: integer);
begin
  if (Index < 0) or (Index >= FCount) then
    // Color index out of range
    Error(sBadColorIndex);
  dec(FCount);
  if (Index < FCount) then
    System.Move(FColorMap^[Index + 1], FColorMap^[Index], (FCount - Index)* sizeof(TGIFColor));
  FOptimized := False;
  Changed;
end;

function TGIFColorMap.GetColor(Index: integer): TColor;
begin
  if (Index < 0) or (Index >= FCount) then
  begin
    // Color index out of range
    Warning(gsWarning, sBadColorIndex);
    // Raise an exception if the color map is empty
    if (FCount = 0) then
      Error(sEmptyColorMap);
    // Default to color index 0
    Index := 0;
  end;
  Result := RGB2Color(FColorMap^[Index]);
end;

procedure TGIFColorMap.SetColor(Index: integer; Value: TColor);
begin
  if (Index < 0) or (Index >= FCount) then
    // Color index out of range
    Error(sBadColorIndex);
  FColorMap^[Index] := Color2RGB(Value);
  Changed;
end;

function TGIFColorMap.DoOptimize: boolean;
var
  Usage			: TColormapHistogram;
  TempMap		: array[0..255] of TGIFColor;
  ReverseMap		: TColormapReverse;
  i			: integer;
  LastFound		: boolean;
  NewCount		: integer;
  T			: TUsageCount;
  Pivot			: integer;

  procedure QuickSort(iLo, iHi: Integer);
  var
    Lo, Hi: Integer;
  begin
    repeat
      Lo := iLo;
      Hi := iHi;
      Pivot := Usage[(iLo + iHi) SHR 1].Count;
      repeat
        while (Usage[Lo].Count - Pivot > 0) do inc(Lo);
        while (Usage[Hi].Count - Pivot < 0) do dec(Hi);
        if (Lo <= Hi) then
        begin
          T := Usage[Lo];
          Usage[Lo] := Usage[Hi];
          Usage[Hi] := T;
          inc(Lo);
          dec(Hi);
        end;
      until (Lo > Hi);
      if (iLo < Hi) then
        QuickSort(iLo, Hi);
      iLo := Lo;
    until (Lo >= iHi);
  end;

begin
  if (FCount <= 1) then
  begin
    Result := False;
    exit;
  end;

  FOptimized := True;
  Result := True;

  BuildHistogram(Usage);

  (*
  **  Sort according to usage count
  *)
  QuickSort(0, FCount-1);

  (*
  ** Test for table already sorted
  *)
  for i := 0 to FCount-1 do
    if (Usage[i].Index <> i) then
      break;
  if (i = FCount) then
    exit;

  (*
  ** Build old to new map
  *)
  for i := 0 to FCount-1 do
    ReverseMap[Usage[i].Index] := i;


  MapImages(ReverseMap);

  (*
  **  Reorder colormap
  *)
  LastFound := False;
  NewCount := FCount;
  Move(FColorMap^, TempMap, FCount * sizeof(TGIFColor));
  for i := 0 to FCount-1 do
  begin
    FColorMap^[ReverseMap[i]] := TempMap[i];
    // Find last used color index
    if (Usage[i].Count = 0) and not(LastFound) then
    begin
      LastFound := True;
      NewCount := i;
    end;
  end;

  FCount := NewCount;

  Changed;
end;

function TGIFColorMap.GetBitsPerPixel: integer;
begin
  Result := Colors2bpp(FCount);
end;

//: Copies one color map to another.
procedure TGIFColorMap.Assign(Source: TPersistent);
begin
  if (Source is TGIFColorMap) then
  begin
    Clear;
    FCapacity := TGIFColorMap(Source).FCapacity;
    FCount := TGIFColorMap(Source).FCount;
    FOptimized := TGIFColorMap(Source).FOptimized;
    FColorMap := AllocMem(FCapacity * sizeof(TGIFColor));
    System.Move(TGIFColorMap(Source).FColorMap^, FColorMap^, FCount * sizeof(TGIFColor));
    Changed;
  end else
    inherited Assign(Source);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFItem
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFItem.Create(GIFImage: TGIFImage);
begin
  inherited Create;

  FGIFImage := GIFImage;
end;

procedure TGIFItem.Warning(Severity: TGIFSeverity; Message: string);
begin
  FGIFImage.Warning(self, Severity, Message);
end;

function TGIFItem.GetVersion: TGIFVersion;
begin
  Result := gv87a;
end;

procedure TGIFItem.LoadFromFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead OR fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGIFItem.SaveToFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFList
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFList.Create(Image: TGIFImage);
begin
  inherited Create;
  FImage := Image;
  FItems := TList.Create;
end;

destructor TGIFList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

function TGIFList.GetItem(Index: Integer): TGIFItem;
begin
  Result := TGIFItem(FItems[Index]);
end;

procedure TGIFList.SetItem(Index: Integer; Item: TGIFItem);
begin
  FItems[Index] := Item;
end;

function TGIFList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TGIFList.Add(Item: TGIFItem): Integer;
begin
  Result := FItems.Add(Item);
end;

procedure TGIFList.Clear;
begin
  while (FItems.Count > 0) do
    Delete(0);
end;

procedure TGIFList.Delete(Index: Integer);
var
  Item			: TGIFItem;
begin
  Item := TGIFItem(FItems[Index]);
  // Delete before item is destroyed to avoid recursion
  FItems.Delete(Index);
  Item.Free;
end;

procedure TGIFList.Exchange(Index1, Index2: Integer);
begin
  FItems.Exchange(Index1, Index2);
end;

function TGIFList.First: TGIFItem;
begin
  Result := TGIFItem(FItems.First);
end;

function TGIFList.IndexOf(Item: TGIFItem): Integer;
begin
  Result := FItems.IndexOf(Item);
end;

procedure TGIFList.Insert(Index: Integer; Item: TGIFItem);
begin
  FItems.Insert(Index, Item);
end;

function TGIFList.Last: TGIFItem;
begin
  Result := TGIFItem(FItems.Last);
end;

procedure TGIFList.Move(CurIndex, NewIndex: Integer);
begin
  FItems.Move(CurIndex, NewIndex);
end;

function TGIFList.Remove(Item: TGIFItem): Integer;
begin
  // Note: TGIFList.Remove must not destroy item
  Result := FItems.Remove(Item);
end;

procedure TGIFList.SaveToStream(Stream: TStream);
var
  i			: integer;
begin
  for i := 0 to FItems.Count-1 do
    TGIFItem(FItems[i]).SaveToStream(Stream);
end;

procedure TGIFList.Warning(Severity: TGIFSeverity; Message: string);
begin
  Image.Warning(self, Severity, Message);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFGlobalColorMap
//
////////////////////////////////////////////////////////////////////////////////
type
  TGIFGlobalColorMap = class(TGIFColorMap)
  private
    FHeader	: TGIFHeader;
  protected
    procedure Warning(Severity: TGIFSeverity; Message: string); override;
    procedure BuildHistogram(var Histogram: TColormapHistogram); override;
    procedure MapImages(var Map: TColormapReverse); override;
  public
    constructor Create(HeaderItem: TGIFHeader);
    function Optimize: boolean; override;
    procedure Changed; override;
  end;

constructor TGIFGlobalColorMap.Create(HeaderItem: TGIFHeader);
begin
  Inherited Create;
  FHeader := HeaderItem;
end;

procedure TGIFGlobalColorMap.Warning(Severity: TGIFSeverity; Message: string);
begin
  FHeader.Image.Warning(self, Severity, Message);
end;

procedure TGIFGlobalColorMap.BuildHistogram(var Histogram: TColormapHistogram);
var
  Pixel			,
  LastPixel		: PChar;
  i			: integer;
begin
  (*
  ** Init histogram
  *)
  for i := 0 to Count-1 do
  begin
    Histogram[i].Index := i;
    Histogram[i].Count := 0;
  end;

  for i := 0 to FHeader.Image.Images.Count-1 do
    if (FHeader.Image.Images[i].ActiveColorMap = self) then
    begin
      Pixel := FHeader.Image.Images[i].Data;
      LastPixel := Pixel + FHeader.Image.Images[i].Width * FHeader.Image.Images[i].Height;

      (*
      ** Sum up usage count for each color
      *)
      while (Pixel < LastPixel) do
      begin
        inc(Histogram[ord(Pixel^)].Count);
        inc(Pixel);
      end;
    end;
end;

procedure TGIFGlobalColorMap.MapImages(var Map: TColormapReverse);
var
  Pixel			,
  LastPixel		: PChar;
  i			: integer;
begin
  for i := 0 to FHeader.Image.Images.Count-1 do
    if (FHeader.Image.Images[i].ActiveColorMap = self) then
    begin
      Pixel := FHeader.Image.Images[i].Data;
      LastPixel := Pixel + FHeader.Image.Images[i].Width * FHeader.Image.Images[i].Height;

      (*
      **  Reorder all pixel to new map
      *)
      while (Pixel < LastPixel) do
      begin
        Pixel^ := chr(Map[ord(Pixel^)]);
        inc(Pixel);
      end;

      (*
      **  Reorder transparent colors
      *)
      if (FHeader.Image.Images[i].Transparent) then
        FHeader.Image.Images[i].GraphicControlExtension.TransparentColorIndex :=
          Map[FHeader.Image.Images[i].GraphicControlExtension.TransparentColorIndex];
    end;
end;

function TGIFGlobalColorMap.Optimize: boolean;
begin
  { Optimize with first image, Remove unused colors if only one image }
  if (FHeader.Image.Images.Count > 0) then
    Result := DoOptimize
  else
    Result := False;
end;

procedure TGIFGlobalColorMap.Changed;
begin
  FHeader.Image.Palette := 0;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFHeader
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFHeader.Create(GIFImage: TGIFImage);
begin
  inherited Create(GIFImage);
  FColorMap := TGIFGlobalColorMap.Create(self);
  Clear;
end;

destructor TGIFHeader.Destroy;
begin
  FColorMap.Free;
  inherited Destroy;
end;

procedure TGIFHeader.Clear;
begin
  FColorMap.Clear;
  FLogicalScreenDescriptor.ScreenWidth := 0;
  FLogicalScreenDescriptor.ScreenHeight := 0;
  FLogicalScreenDescriptor.PackedFields := 0;
  FLogicalScreenDescriptor.BackgroundColorIndex := 0;
  FLogicalScreenDescriptor.AspectRatio := 0;
end;

procedure TGIFHeader.Assign(Source: TPersistent);
begin
  if (Source is TGIFHeader) then
  begin
    ColorMap.Assign(TGIFHeader(Source).ColorMap);
    FLogicalScreenDescriptor := TGIFHeader(Source).FLogicalScreenDescriptor;
  end else
  if (Source is TGIFColorMap) then
  begin
    Clear;
    ColorMap.Assign(TGIFColorMap(Source));
  end else
    inherited Assign(Source);
end;

type
  TGIFHeaderRec = packed record
    Signature: array[0..2] of char; { contains 'GIF' }
    Version: TGIFVersionRec;   { '87a' or '89a' }
  end;

const
  { logical screen descriptor packed field masks }
  lsdGlobalColorTable	= $80;		{ set if global color table follows L.S.D. }
  lsdColorResolution	= $70;		{ Color resolution - 3 bits }
  lsdSort		= $08;		{ set if global color table is sorted - 1 bit }
  lsdColorTableSize	= $07;		{ size of global color table - 3 bits }
  					{ Actual size = 2^value+1    - value is 3 bits }
procedure TGIFHeader.Prepare;
var
  pack			: BYTE;
begin
  Pack := $00;
  if (ColorMap.Count > 0) then
  begin
    Pack := lsdGlobalColorTable;
    if (ColorMap.Optimized) then
      Pack := Pack OR lsdSort;
  end;
  // Note: The SHL below was SHL 5 in the original source, but that looks wrong
  Pack := Pack OR ((Image.ColorResolution SHL 4) AND lsdColorResolution);
  Pack := Pack OR ((Image.BitsPerPixel-1) AND lsdColorTableSize);
  FLogicalScreenDescriptor.PackedFields := Pack;
end;

procedure TGIFHeader.SaveToStream(Stream: TStream);
var
  GifHeader		: TGIFHeaderRec;
  v			: TGIFVersion;
begin
  v := Image.Version;
  if (v = gvUnknown) then
    Error(sBadVersion);

  GifHeader.Signature := 'GIF';
  GifHeader.Version := GIFVersions[v];

  Prepare;
  Stream.Write(GifHeader, sizeof(GifHeader));
  Stream.Write(FLogicalScreenDescriptor, sizeof(FLogicalScreenDescriptor));
  if (FLogicalScreenDescriptor.PackedFields AND lsdGlobalColorTable = lsdGlobalColorTable) then
    ColorMap.SaveToStream(Stream);
end;

procedure TGIFHeader.LoadFromStream(Stream: TStream);
var
  GifHeader		: TGIFHeaderRec;
  ColorCount		: integer;
  Position		: integer;
begin
  Position := Stream.Position;

  ReadCheck(Stream, GifHeader, sizeof(GifHeader));
  if (uppercase(GifHeader.Signature) <> 'GIF') then
  begin
    // Attempt recovery in case we are reading a GIF stored in a form by rxLib
    Stream.Position := Position;
    // Seek past size stored in stream
    Stream.Seek(sizeof(longInt), soFromCurrent);
    // Attempt to read signature again
    ReadCheck(Stream, GifHeader, sizeof(GifHeader));
    if (uppercase(GifHeader.Signature) <> 'GIF') then
      Error(sBadSignature);
  end;

  ReadCheck(Stream, FLogicalScreenDescriptor, sizeof(FLogicalScreenDescriptor));

  if (FLogicalScreenDescriptor.PackedFields AND lsdGlobalColorTable = lsdGlobalColorTable) then
  begin
    ColorCount := 2 SHL (FLogicalScreenDescriptor.PackedFields AND lsdColorTableSize);
    if (ColorCount < 2) or (ColorCount > 256) then
      Error(sScreenBadColorSize);
    ColorMap.LoadFromStream(Stream, ColorCount)
  end else
    ColorMap.Clear;
end;

function TGIFHeader.GetVersion: TGIFVersion;
begin
  if (FColorMap.Optimized) or (AspectRatio <> 0) then
    Result := gv89a
  else
    Result := inherited GetVersion;
end;

function TGIFHeader.GetBackgroundColor: TColor;
begin
  Result := FColorMap[BackgroundColorIndex];
end;

procedure TGIFHeader.SetBackgroundColor(Color: TColor);
begin
  BackgroundColorIndex := FColorMap.AddUnique(Color);
end;

procedure TGIFHeader.SetBackgroundColorIndex(Index: BYTE);
begin
  if ((Index >= FColorMap.Count) and (FColorMap.Count > 0)) then
  begin
    Warning(gsWarning, sBadColorIndex);
    Index := 0;
  end;
  FLogicalScreenDescriptor.BackgroundColorIndex := Index;
end;

function TGIFHeader.GetBitsPerPixel: integer;
begin
  Result := FColorMap.BitsPerPixel;
end;

function TGIFHeader.GetColorResolution: integer;
begin
  Result := FColorMap.BitsPerPixel-1;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFLocalColorMap
//
////////////////////////////////////////////////////////////////////////////////
type
  TGIFLocalColorMap = class(TGIFColorMap)
  private
    FSubImage		: TGIFSubImage;
  protected
    procedure Warning(Severity: TGIFSeverity; Message: string); override;
    procedure BuildHistogram(var Histogram: TColormapHistogram); override;
    procedure MapImages(var Map: TColormapReverse); override;
  public
    constructor Create(SubImage: TGIFSubImage);
    function Optimize: boolean; override;
    procedure Changed; override;
  end;

constructor TGIFLocalColorMap.Create(SubImage: TGIFSubImage);
begin
  Inherited Create;
  FSubImage := SubImage;
end;

procedure TGIFLocalColorMap.Warning(Severity: TGIFSeverity; Message: string);
begin
  FSubImage.Image.Warning(self, Severity, Message);
end;

procedure TGIFLocalColorMap.BuildHistogram(var Histogram: TColormapHistogram);
var
  Pixel			,
  LastPixel		: PChar;
  i			: integer;
begin
  Pixel := FSubImage.Data;
  LastPixel := Pixel + FSubImage.Width * FSubImage.Height;

  (*
  ** Init histogram
  *)
  for i := 0 to Count-1 do
  begin
    Histogram[i].Index := i;
    Histogram[i].Count := 0;
  end;

  (*
  ** Sum up usage count for each color
  *)
  while (Pixel < LastPixel) do
  begin
    inc(Histogram[ord(Pixel^)].Count);
    inc(Pixel);
  end;
end;

procedure TGIFLocalColorMap.MapImages(var Map: TColormapReverse);
var
  Pixel			,
  LastPixel		: PChar;
begin
  Pixel := FSubImage.Data;
  LastPixel := Pixel + FSubImage.Width * FSubImage.Height;

  (*
  **  Reorder all pixel to new map
  *)
  while (Pixel < LastPixel) do
  begin
    Pixel^ := chr(Map[ord(Pixel^)]);
    inc(Pixel);
  end;

  (*
  **  Reorder transparent colors
  *)
  if (FSubImage.Transparent) then
    FSubImage.GraphicControlExtension.TransparentColorIndex :=
      Map[FSubImage.GraphicControlExtension.TransparentColorIndex];
end;

function TGIFLocalColorMap.Optimize: boolean;
begin
  Result := DoOptimize;
end;

procedure TGIFLocalColorMap.Changed;
begin
  FSubImage.Palette := 0;
end;


////////////////////////////////////////////////////////////////////////////////
//
//			LZW Decoder
//
////////////////////////////////////////////////////////////////////////////////
const
  GIFCodeBits		= 12;			// Max number of bits per GIF token code
  GIFCodeMax		= (1 SHL GIFCodeBits)-1;// Max GIF token code
  						// 12 bits = 4095
  StackSize		= (2 SHL GIFCodeBits);	// Size of decompression stack
  TableSize		= (1 SHL GIFCodeBits);	// Size of decompression table

procedure TGIFSubImage.Decompress(Stream: TStream);
var
  table0		: array[0..TableSize-1] of integer;
  table1		: array[0..TableSize-1] of integer;
  firstcode, oldcode	: integer;
  buf			: array[0..257] of BYTE;

  Dest			: PChar;
  v			,
  xpos, ypos, pass	: integer;

  stack			: array[0..StackSize-1] of integer;
  Source			: ^integer;
  BitsPerCode		: integer;		// number of CodeTableBits/code
  InitialBitsPerCode	: BYTE;

  MaxCode		: integer;		// maximum code, given BitsPerCode
  MaxCodeSize		: integer;
  ClearCode		: integer;		// Special code to signal "Clear table"
  EOFCode		: integer;		// Special code to signal EOF
  step			: integer;
  i			: integer;

  StartBit		,			// Index of bit buffer start
  LastBit		,			// Index of last bit in buffer
  LastByte		: integer;		// Index of last byte in buffer
  get_done		,
  return_clear		,
  ZeroBlock		: boolean;
  ClearValue		: BYTE;
{$ifdef DEBUG_DECOMPRESSPERFORMANCE}
  TimeStartDecompress	,
  TimeStopDecompress	: DWORD;
{$endif}

  function nextCode(BitsPerCode: integer): integer;
  const
    masks: array[0..15] of integer =
      ($0000, $0001, $0003, $0007,
       $000f, $001f, $003f, $007f,
       $00ff, $01ff, $03ff, $07ff,
       $0fff, $1fff, $3fff, $7fff);
  var
    StartIndex, EndIndex		: integer;
    ret			: integer;
    EndBit		: integer;
    count		: BYTE;
  begin
    if (return_clear) then
    begin
      return_clear := False;
      Result := ClearCode;
      exit;
    end;

    EndBit := StartBit + BitsPerCode;

    if (EndBit >= LastBit) then
    begin
      if (get_done) then
      begin
        if (StartBit >= LastBit) then
          Warning(gsWarning, sDecodeTooFewBits);
        Result := -1;
        exit;
      end;
      buf[0] := buf[LastByte-2];
      buf[1] := buf[LastByte-1];

      if (Stream.Read(count, 1) <> 1) then
      begin
        Result := -1;
        exit;
      end;
      if (count = 0) then
      begin
        ZeroBlock := True;
        get_done := TRUE;
      end else
      begin
        // Handle premature end of file
        if (Stream.Size - Stream.Position < Count) then
        begin
          Warning(gsWarning, sOutOfData);
          // Not enough data left - Just read as much as we can get
          Count := Stream.Size - Stream.Position;
        end;
        if (Count <> 0) then
          ReadCheck(Stream, Buf[2], Count);
      end;

      LastByte := 2 + count;
      StartBit := (StartBit - LastBit) + 16;
      LastBit := LastByte * 8;

      EndBit := StartBit + BitsPerCode;
    end;

    EndIndex := EndBit DIV 8;
    StartIndex := StartBit DIV 8;

    ASSERT(StartIndex <= high(buf), 'StartIndex too large');
    if (StartIndex = EndIndex) then
      ret := buf[StartIndex]
    else
      if (StartIndex + 1 = EndIndex) then
        ret := buf[StartIndex] OR (buf[StartIndex+1] SHL 8)
      else
        ret := buf[StartIndex] OR (buf[StartIndex+1] SHL 8) OR (buf[StartIndex+2] SHL 16);

    ret := (ret SHR (StartBit AND $0007)) AND masks[BitsPerCode];

    Inc(StartBit, BitsPerCode);

    Result := ret;
  end;

  function NextLZW: integer;
  var
    code, incode	: integer;
    i			: integer;
    b			: BYTE;
  begin
    code := nextCode(BitsPerCode);
    while (code >= 0) do
    begin
      if (code = ClearCode) then
      begin
        ASSERT(ClearCode < TableSize, 'ClearCode too large');
        for i := 0 to ClearCode-1 do
        begin
          table0[i] := 0;
          table1[i] := i;
        end;
        for i := ClearCode to TableSize-1 do
        begin
          table0[i] := 0;
          table1[i] := 0;
        end;
        BitsPerCode := InitialBitsPerCode+1;
        MaxCodeSize := 2 * ClearCode;
        MaxCode := ClearCode + 2;
        Source := @stack;
        repeat
          firstcode := nextCode(BitsPerCode);
          oldcode := firstcode;
        until (firstcode <> ClearCode);

        Result := firstcode;
        exit;
      end;
      if (code = EOFCode) then
      begin
        Result := -2;
        if (ZeroBlock) then
          exit;
        // Eat rest of data blocks
        if (Stream.Read(b, 1) <> 1) then
          exit;
        while (b <> 0) do
        begin
          Stream.Seek(b, soFromCurrent);
          if (Stream.Read(b, 1) <> 1) then
            exit;
        end;
        exit;
      end;

      incode := code;

      if (code >= MaxCode) then
      begin
        Source^ := firstcode;
        Inc(Source);
        code := oldcode;
      end;

      ASSERT(Code < TableSize, 'Code too large');
      while (code >= ClearCode) do
      begin
        Source^ := table1[code];
        Inc(Source);
        if (code = table0[code]) then
          Error(sDecodeCircular);
        code := table0[code];
        ASSERT(Code < TableSize, 'Code too large');
      end;

      firstcode := table1[code];
      Source^ := firstcode;
      Inc(Source);

      code := MaxCode;
      if (code <= GIFCodeMax) then
      begin
        table0[code] := oldcode;
        table1[code] := firstcode;
        Inc(MaxCode);
        if ((MaxCode >= MaxCodeSize) and (MaxCodeSize <= GIFCodeMax)) then
        begin
          MaxCodeSize := MaxCodeSize * 2;
          Inc(BitsPerCode);
        end;
      end;

      oldcode := incode;

      if (longInt(Source) > longInt(@stack)) then
      begin
        Dec(Source);
        Result := Source^;
        exit;
      end
    end;
    Result := code;
  end;

  function readLZW: integer;
  begin
    if (longInt(Source) > longInt(@stack)) then
    begin
      Dec(Source);
      Result := Source^;
    end else
      Result := NextLZW;
  end;

begin
  NewImage;

  // Clear image data in case decompress doesn't complete
  if (Transparent) then
    // Clear to transparent color
    ClearValue := GraphicControlExtension.GetTransparentColorIndex
  else
    // Clear to first color
    ClearValue := 0;

  FillChar(FData^, FDataSize, ClearValue);

{$ifdef DEBUG_DECOMPRESSPERFORMANCE}
  TimeStartDecompress := timeGetTime;
{$endif}

  (*
  ** Read initial code size in bits from stream
  *)
  if (Stream.Read(InitialBitsPerCode, 1) <> 1) then
    exit;

  (*
  **  Initialize the Compression routines
  *)
  BitsPerCode := InitialBitsPerCode + 1;
  ClearCode := 1 SHL InitialBitsPerCode;
  EOFCode := ClearCode + 1;
  MaxCodeSize := 2 * ClearCode;
  MaxCode := ClearCode + 2;

  StartBit := 0;
  LastBit := 0;
  LastByte := 2;

  ZeroBlock := False;
  get_done := False;
  return_clear := TRUE;

  Source := @stack;

  try
    if (Interlaced) then
    begin
      ypos := 0;
      pass := 0;
      step := 8;

      for i := 0 to Height-1 do
      begin
        Dest := FData + Width * ypos;
        for xpos := 0 to width-1 do
        begin
          v := readLZW;
          if (v < 0) then
            exit;
          Dest^ := char(v);
          Inc(Dest);
        end;
        Inc(ypos, step);
        if (ypos >= height) then
          repeat
            if (pass > 0) then
              step := step DIV 2;
            Inc(pass);
            ypos := step DIV 2;
          until (ypos < height);
      end;
    end else
    begin
      Dest := FData;
      for ypos := 0 to (height * width)-1 do
      begin
        v := readLZW;
        if (v < 0) then
          exit;
        Dest^ := char(v);
        Inc(Dest);
      end;
    end;
  finally
    if (readLZW >= 0) then
      ;
//      raise GIFException.Create('Too much input data, ignoring extra...');
  end;
{$ifdef DEBUG_DECOMPRESSPERFORMANCE}
  TimeStopDecompress := timeGetTime;
  ShowMessage(format('Decompressed %d pixels in %d mS, Rate %d pixels/mS',
    [Height*Width, TimeStopDecompress-TimeStartDecompress,
    (Height*Width) DIV (TimeStopDecompress-TimeStartDecompress+1)]));
{$endif}
end;

////////////////////////////////////////////////////////////////////////////////
//
//			LZW Encoder stuff
//
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
//			LZW Encoder THashTable
////////////////////////////////////////////////////////////////////////////////
const
  HashKeyBits		= 13;			// Max number of bits per Hash Key

  HashSize		= 8009;			// Size of hash table
  						// Must be prime
                                                // Must be > than HashMaxCode
                                                // Must be < than HashMaxKey

  HashKeyMax		= (1 SHL HashKeyBits)-1;// Max hash key value
  						// 13 bits = 8191

  HashKeyMask		= HashKeyMax;		// $1FFF
  GIFCodeMask		= GIFCodeMax;		// $0FFF

  HashEmpty		= $000FFFFF;		// 20 bits

type
  // A Hash Key is 20 bits wide.
  // - The lower 8 bits are the postfix character (the new pixel).
  // - The upper 12 bits are the prefix code (the GIF token).
  // A KeyInt must be able to represent the integer values -1..(2^20)-1
  KeyInt = longInt;	// 32 bits
  CodeInt = SmallInt;	// 16 bits

  THashArray = array[0..HashSize-1] of KeyInt;
  PHashArray = ^THashArray;

  THashTable = class
{$ifdef DEBUG_HASHPERFORMANCE}
    CountLookupFound	: longInt;
    CountMissFound	: longInt;
    CountLookupNotFound	: longInt;
    CountMissNotFound	: longInt;
{$endif}
    HashTable: PHashArray;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Insert(Key: KeyInt; Code: CodeInt);
    function Lookup(Key: KeyInt): CodeInt;
  end;

function HashKey(Key: KeyInt): CodeInt;
begin
  Result := ((Key SHR (GIFCodeBits-8)) XOR Key) MOD HashSize;
end;

function NextHashKey(HKey: CodeInt): CodeInt;
var
  disp		: CodeInt;
begin
  (*
  ** secondary hash (after G. Knott)
  *)
  disp := HashSize - HKey;
  if (HKey = 0) then
    disp := 1;
//  disp := 13;		// disp should be prime relative to HashSize, but
			// it doesn't seem to matter here...
  dec(HKey, disp);
  if (HKey < 0) then
    inc(HKey, HashSize);
  Result := HKey;
end;


constructor THashTable.Create;
begin
  ASSERT(longInt($FFFFFFFF) = -1, 'TGIFImage implementation assumes $FFFFFFFF = -1');

  inherited Create;
  GetMem(HashTable, sizeof(THashArray));
  Clear;
{$ifdef DEBUG_HASHPERFORMANCE}
  CountLookupFound := 0;
  CountMissFound := 0;
  CountLookupNotFound := 0;
  CountMissNotFound := 0;
{$endif}
end;

destructor THashTable.Destroy;
begin
{$ifdef DEBUG_HASHPERFORMANCE}
  ShowMessage(
    Format('Found: %d  HitRate: %.2f',
      [CountLookupFound, (CountLookupFound+1)/(CountMissFound+1)])+#13+
    Format('Not found: %d  HitRate: %.2f',
      [CountLookupNotFound, (CountLookupNotFound+1)/(CountMissNotFound+1)]));
{$endif}
  FreeMem(HashTable);
  inherited Destroy;
end;

// Clear hash table and fill with empty slots (doh!)
procedure THashTable.Clear;
{$ifdef DEBUG_HASHFILLFACTOR}
var
  i			,
  Count			: longInt;
{$endif}
begin
{$ifdef DEBUG_HASHFILLFACTOR}
  Count := 0;
  for i := 0 to HashSize-1 do
    if (HashTable[i] SHR GIFCodeBits <> HashEmpty) then
      inc(Count);
  ShowMessage(format('Size: %d, Filled: %d, Rate %.4f',
    [HashSize, Count, Count/HashSize]));
{$endif}

  FillChar(HashTable^, sizeof(THashArray), $FF);
end;

// Insert new key/value pair into hash table
procedure THashTable.Insert(Key: KeyInt; Code: CodeInt);
var
  HKey			: CodeInt;
begin
  // Create hash key from prefix string
  HKey := HashKey(Key);

  // Scan for empty slot
  // while (HashTable[HKey] SHR GIFCodeBits <> HashEmpty) do { Unoptimized }
  while (HashTable[HKey] AND (HashEmpty SHL GIFCodeBits) <> (HashEmpty SHL GIFCodeBits)) do { Optimized }
    HKey := NextHashKey(HKey);
  // Fill slot with key/value pair
  HashTable[HKey] := (Key SHL GIFCodeBits) OR (Code AND GIFCodeMask);
end;

// Search for key in hash table.
// Returns value if found or -1 if not
function THashTable.Lookup(Key: KeyInt): CodeInt;
var
  HKey			: CodeInt;
  HTKey			: KeyInt;
{$ifdef DEBUG_HASHPERFORMANCE}
  n			: LongInt;
{$endif}
begin
  // Create hash key from prefix string
  HKey := HashKey(Key);

{$ifdef DEBUG_HASHPERFORMANCE}
  n := 0;
{$endif}
  // Scan table for key
  // HTKey := HashTable[HKey] SHR GIFCodeBits; { Unoptimized }
  Key := Key SHL GIFCodeBits; { Optimized }
  HTKey := HashTable[HKey] AND (HashEmpty SHL GIFCodeBits); { Optimized }
  // while (HTKey <> HashEmpty) do { Unoptimized }
  while (HTKey <> HashEmpty SHL GIFCodeBits) do { Optimized }
  begin
    if (Key = HTKey) then
    begin
      // Extract and return value
      Result := HashTable[HKey] AND GIFCodeMask;
{$ifdef DEBUG_HASHPERFORMANCE}
      inc(CountLookupFound);
      inc(CountMissFound, n);
{$endif}
      exit;
    end;
{$ifdef DEBUG_HASHPERFORMANCE}
    inc(n);
{$endif}
    // Try next slot
    HKey := NextHashKey(HKey);
    // HTKey := HashTable[HKey] SHR GIFCodeBits; { Unoptimized }
    HTKey := HashTable[HKey] AND (HashEmpty SHL GIFCodeBits); { Optimized }
  end;
  // Found empty slot - key doesn't exist
  Result := -1;
{$ifdef DEBUG_HASHPERFORMANCE}
  inc(CountLookupNotFound);
  inc(CountMissNotFound, n);
{$endif}
end;

////////////////////////////////////////////////////////////////////////////////
//		TGIFStream - Abstract GIF block stream
//
// Descendants from TGIFStream either reads or writes data in blocks
// of up to 255 bytes. These blocks are organized as a leading byte
// containing the number of bytes in the block (exclusing the count
// byte itself), followed by the data (up to 254 bytes of data).
////////////////////////////////////////////////////////////////////////////////
type
  TGIFStream = class(TStream)
  private
    FOnWarning		: TGIFWarning;
    FStream		: TStream;
    FOnProgress		: TNotifyEvent;
    FBuffer		: array [BYTE] of Char;
    FBufferCount	: integer;

  protected
    constructor Create(Stream: TStream);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    procedure Progress(Sender: TObject); dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  public
    property Warning: TGIFWarning read FOnWarning write FOnWarning;
  end;

constructor TGIFStream.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FBufferCount := 1; // Reserve first byte of buffer for length
end;

procedure TGIFStream.Progress(Sender: TObject);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender);
end;

function TGIFStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.Create(sInvalidStream);
end;

function TGIFStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise Exception.Create(sInvalidStream);
end;

function TGIFStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise Exception.Create(sInvalidStream);
end;

////////////////////////////////////////////////////////////////////////////////
//		TGIFReader - GIF block reader
////////////////////////////////////////////////////////////////////////////////
type
  TGIFReader = class(TGIFStream)
  public
    constructor Create(Stream: TStream);

    function Read(var Buffer; Count: Longint): Longint; override;
  end;

constructor TGIFReader.Create(Stream: TStream);
begin
  inherited Create(Stream);
  FBufferCount := 0;
end;

function TGIFReader.Read(var Buffer; Count: Longint): Longint;
var
  n			: integer;
  Dst			: PChar;
  size			: BYTE;
begin
  Dst := @Buffer;
  Result := 0;

  while (Count > 0) do
  begin
    // Get data from buffer
    while (FBufferCount > 0) and (Count > 0) do
    begin
      if (FBufferCount > Count) then
        n := Count
      else
        n := FBufferCount;
      Move(FBuffer, Dst^, n);
      dec(FBufferCount, n);
      dec(Count, n);
      inc(Result, n);
      inc(Dst, n);
    end;

    // Refill buffer when it becomes empty
    if (FBufferCount <= 0) then
    begin
      FStream.Read(size, 1);
      { TODO -oanme -cImprovement : Should be handled as a warning instead of an error. }
      if (size >= 255) then
        Error('GIF block too large');
      FBufferCount := size;
      if (FBufferCount > 0) then
      begin
        n := FStream.Read(FBuffer, size);
        if (n = FBufferCount) then
        begin
          Warning(self, gsWarning, sOutOfData);
          break;
        end;
      end else
        break;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//		TGIFWriter - GIF block writer
////////////////////////////////////////////////////////////////////////////////
type
  TGIFWriter = class(TGIFStream)
  private
    FOutputDirty	: boolean;

  protected
    procedure FlushBuffer;

  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;

    function Write(const Buffer; Count: Longint): Longint; override;
    function WriteByte(Value: BYTE): Longint;
  end;

constructor TGIFWriter.Create(Stream: TStream);
begin
  inherited Create(Stream);
  FBufferCount := 1; // Reserve first byte of buffer for length
  FOutputDirty := False;
end;

destructor TGIFWriter.Destroy;
begin
  inherited Destroy;
  if (FOutputDirty) then
    FlushBuffer;
end;

procedure TGIFWriter.FlushBuffer;
begin
  if (FBufferCount <= 0) then
    exit;

  FBuffer[0] := char(FBufferCount-1); // Block size excluding the count
  FStream.WriteBuffer(FBuffer, FBufferCount);
  FBufferCount := 1; // Reserve first byte of buffer for length
  FOutputDirty := False;
end;

function TGIFWriter.Write(const Buffer; Count: Longint): Longint;
var
  n			: integer;
  Src			: PChar;
begin
  Result := Count;
  FOutputDirty := True;
  Src := @Buffer;
  while (Count > 0) do
  begin
    // Move data to the internal buffer in 255 byte chunks
    while (FBufferCount < sizeof(FBuffer)) and (Count > 0) do
    begin
      n := sizeof(FBuffer) - FBufferCount;
      if (n > Count) then
        n := Count;
      Move(Src^, FBuffer[FBufferCount], n);
      inc(Src, n);
      inc(FBufferCount, n);
      dec(Count, n);
    end;

    // Flush the buffer when it is full
    if (FBufferCount >= sizeof(FBuffer)) then
      FlushBuffer;
  end;
end;

function TGIFWriter.WriteByte(Value: BYTE): Longint;
begin
  Result := Write(Value, 1);
end;

////////////////////////////////////////////////////////////////////////////////
//		TGIFEncoder - Abstract encoder
////////////////////////////////////////////////////////////////////////////////
type
  TGIFEncoder = class(TObject)
  protected
    FOnWarning		: TGIFWarning;
    MaxColor		: integer;
    BitsPerPixel	: BYTE;		// Bits per pixel of image
    Stream		: TStream;	// Output stream
    Width		,		// Width of image in pixels
    Height		: integer;	// height of image in pixels
    Interlace		: boolean;	// Interlace flag (True = interlaced image)
    Data		: PChar;	// Pointer to pixel data
    GIFStream		: TGIFWriter;	// Output buffer

    OutputBucket	: longInt;	// Output bit bucket
    OutputBits		: integer;	// Current # of bits in bucket

    ClearFlag		: Boolean;	// True if dictionary has just been cleared
    BitsPerCode		,		// Current # of bits per code
    InitialBitsPerCode	: integer;	// Initial # of bits per code after
  					// dictionary has been cleared
    MaxCode		: CodeInt;	// maximum code, given BitsPerCode
    ClearCode		: CodeInt;	// Special output code to signal "Clear table"
    EOFCode		: CodeInt;	// Special output code to signal EOF
    BaseCode		: CodeInt;	// ...

    Pixel		: PChar;	// Pointer to current pixel

    cX			,		// Current X counter (Width - X)
    Y			: integer;	// Current Y
    Pass		: integer;	// Interlace pass

    function MaxCodesFromBits(Bits: integer): CodeInt;
    procedure Output(Value: integer); virtual;
    procedure Clear; virtual;
    function BumpPixel: boolean;
    procedure DoCompress; virtual; abstract;
  public
    procedure Compress(AStream: TStream; ABitsPerPixel: integer;
      AWidth, AHeight: integer; AInterlace: boolean; AData: PChar; AMaxColor: integer);
    property Warning: TGIFWarning read FOnWarning write FOnWarning;
  end;

// Calculate the maximum number of codes that a given number of bits can represent
// MaxCodes := (1^bits)-1
function TGIFEncoder.MaxCodesFromBits(Bits: integer): CodeInt;
begin
  Result := (CodeInt(1) SHL Bits) - 1;
end;

// Stuff bits (variable sized codes) into a buffer and output them
// a byte at a time
procedure TGIFEncoder.Output(Value: integer);
const
  BitBucketMask: array[0..16] of longInt =
    ($0000,
     $0001, $0003, $0007, $000F,
     $001F, $003F, $007F, $00FF,
     $01FF, $03FF, $07FF, $0FFF,
     $1FFF, $3FFF, $7FFF, $FFFF);
begin
  if (OutputBits > 0) then
    OutputBucket :=
      (OutputBucket AND BitBucketMask[OutputBits]) OR (longInt(Value) SHL OutputBits)
  else
    OutputBucket := Value;

  inc(OutputBits, BitsPerCode);

  while (OutputBits >= 8) do
  begin
    GIFStream.WriteByte(OutputBucket AND $FF);
    OutputBucket := OutputBucket SHR 8;
    dec(OutputBits, 8);
  end;

  if (Value = EOFCode) then
  begin
    // At EOF, write the rest of the buffer.
    while (OutputBits > 0) do
    begin
      GIFStream.WriteByte(OutputBucket AND $FF);
      OutputBucket := OutputBucket SHR 8;
      dec(OutputBits, 8);
    end;
  end;
end;

procedure TGIFEncoder.Clear;
begin
  // just_cleared = 1;
  ClearFlag := TRUE;
  Output(ClearCode);
end;

// Bump (X,Y) and data pointer to point to the next pixel
function TGIFEncoder.BumpPixel: boolean;
begin
  // Bump the current X position
  dec(cX);

  // If we are at the end of a scan line, set cX back to the beginning
  // If we are interlaced, bump Y to the appropriate spot, otherwise,
  // just increment it.
  if (cX <= 0) then
  begin

    if not(Interlace) then
    begin
      // Done - no more data
      Result := False;
      exit;
    end;

    cX := Width;
    case (Pass) of
      0:
        begin
          inc(Y, 8);
          if (Y >= Height) then
          begin
            inc(Pass);
            Y := 4;
          end;
        end;
      1:
        begin
          inc(Y, 8);
          if (Y >= Height) then
          begin
            inc(Pass);
            Y := 2;
          end;
        end;
      2:
        begin
          inc(Y, 4);
          if (Y >= Height) then
          begin
            inc(Pass);
            Y := 1;
          end;
        end;
      3:
        inc(Y, 2);
    end;

    if (Y >= height) then
    begin
      // Done - No more data
      Result := False;
      exit;
    end;
    Pixel := Data + (Y * Width);
  end;
  Result := True;
end;


procedure TGIFEncoder.Compress(AStream: TStream; ABitsPerPixel: integer;
  AWidth, AHeight: integer; AInterlace: boolean; AData: PChar; AMaxColor: integer);
const
  EndBlockByte		= $00;			// End of block marker
{$ifdef DEBUG_COMPRESSPERFORMANCE}
var
  TimeStartCompress	,
  TimeStopCompress	: DWORD;
{$endif}
begin
  MaxColor := AMaxColor;
  Stream := AStream;
  BitsPerPixel := ABitsPerPixel;
  Width := AWidth;
  Height := AHeight;
  Interlace := AInterlace;
  Data := AData;

  if (BitsPerPixel <= 1) then
    BitsPerPixel := 2;

  InitialBitsPerCode := BitsPerPixel + 1;
  Stream.Write(BitsPerPixel, 1);

  // out_bits_init = init_bits;
  BitsPerCode := InitialBitsPerCode;
  MaxCode := MaxCodesFromBits(BitsPerCode);

  ClearCode := (1 SHL (InitialBitsPerCode - 1));
  EOFCode := ClearCode + 1;
  BaseCode := EOFCode + 1;

  // Clear bit bucket
  OutputBucket := 0;
  OutputBits  := 0;

  // Reset pixel counter
  if (Interlace) then
    cX := Width
  else
    cX := Width*Height;
  // Reset row counter
  Y := 0;
  Pass := 0;

  GIFStream := TGIFWriter.Create(AStream);
  try
    GIFStream.Warning := Warning;
    if (Data <> nil) and (Height > 0) and (Width > 0) then
    begin
{$ifdef DEBUG_COMPRESSPERFORMANCE}
      TimeStartCompress := timeGetTime;
{$endif}

      // Call compress implementation
      DoCompress;

{$ifdef DEBUG_COMPRESSPERFORMANCE}
      TimeStopCompress := timeGetTime;
      ShowMessage(format('Compressed %d pixels in %d mS, Rate %d pixels/mS',
        [Height*Width, TimeStopCompress-TimeStartCompress,
        DWORD(Height*Width) DIV (TimeStopCompress-TimeStartCompress+1)]));
{$endif}
      // Output the final code.
      Output(EOFCode);
    end else
      // Output the final code (and nothing else).
      TGIFEncoder(self).Output(EOFCode);
  finally
    GIFStream.Free;
  end;

  WriteByte(Stream, EndBlockByte);
end;

////////////////////////////////////////////////////////////////////////////////
//		TRLEEncoder - RLE encoder
////////////////////////////////////////////////////////////////////////////////
type
  TRLEEncoder = class(TGIFEncoder)
  private
    MaxCodes		: integer;
    OutBumpInit		,
    OutClearInit	: integer;
    Prefix		: integer;	// Current run color
    RunLengthTableMax	,
    RunLengthTablePixel	,
    OutCount		,
    OutClear		,
    OutBump		: integer;
  protected
    function ComputeTriangleCount(count: integer; nrepcodes: integer): integer;
    procedure MaxOutClear;
    procedure ResetOutClear;
    procedure FlushFromClear(Count: integer);
    procedure FlushClearOrRepeat(Count: integer);
    procedure FlushWithTable(Count: integer);
    procedure Flush(RunLengthCount: integer);
    procedure OutputPlain(Value: integer);
    procedure Clear; override;
    procedure DoCompress; override;
  end;


procedure TRLEEncoder.Clear;
begin
  OutBump := OutBumpInit;
  OutClear := OutClearInit;
  OutCount := 0;
  RunLengthTableMax := 0;

  inherited Clear;

  BitsPerCode := InitialBitsPerCode;
end;

procedure TRLEEncoder.OutputPlain(Value: integer);
begin
  ClearFlag := False;
  Output(Value);
  inc(OutCount);

  if (OutCount >= OutBump) then
  begin
    inc(BitsPerCode);
    inc(OutBump, 1 SHL (BitsPerCode - 1));
  end;

  if (OutCount >= OutClear) then
    Clear;
end;

function TRLEEncoder.ComputeTriangleCount(count: integer; nrepcodes: integer): integer;
var
  PerRepeat		: integer;
  n			: integer;

  function iSqrt(x: integer): integer;
  var
    r, v		: integer;
  begin
    if (x < 2) then
    begin
      Result := x;
      exit;
    end else
    begin
      v := x;
      r := 1;
      while (v > 0) do
      begin
        v := v DIV 4;
        r := r * 2;
      end;
    end;

    while (True) do
    begin
      v := ((x DIV r) + r) DIV 2;
      if ((v = r) or (v = r+1)) then
      begin
        Result := r;
        exit;
      end;
      r := v;
    end;
  end;

begin
  Result := 0;
  PerRepeat := (nrepcodes * (nrepcodes+1)) DIV 2;

  while (Count >= PerRepeat) do
  begin
    inc(Result, nrepcodes);
    dec(Count, PerRepeat);
  end;

  if (Count > 0) then
  begin
    n := iSqrt(Count);
    while ((n * (n+1)) >= 2*Count) do
      dec(n);
    while ((n * (n+1)) < 2*Count) do
      inc(n);
    inc(Result, n);
  end;
end;

procedure TRLEEncoder.MaxOutClear;
begin
  OutClear := MaxCodes;
end;

procedure TRLEEncoder.ResetOutClear;
begin
  OutClear := OutClearInit;
  if (OutCount >= OutClear) then
    Clear;
end;

procedure TRLEEncoder.FlushFromClear(Count: integer);
var
  n			: integer;
begin
  MaxOutClear;
  RunLengthTablePixel := Prefix;
  n := 1;
  while (Count > 0) do
  begin
    if (n = 1) then
    begin
      RunLengthTableMax := 1;
      OutputPlain(Prefix);
      dec(Count);
    end else
    if (Count >= n) then
    begin
      RunLengthTableMax := n;
      OutputPlain(BaseCode + n - 2);
      dec(Count, n);
    end else
    if (Count = 1) then
    begin
      inc(RunLengthTableMax);
      OutputPlain(Prefix);
      break;
    end else
    begin
      inc(RunLengthTableMax);
      OutputPlain(BaseCode + Count - 2);
      break;
    end;

    if (OutCount = 0) then
      n := 1
    else
      inc(n);
  end;
  ResetOutClear;
end;

procedure TRLEEncoder.FlushClearOrRepeat(Count: integer);
var
  WithClear		: integer;
begin
  WithClear := 1 + ComputeTriangleCount(Count, MaxCodes);

  if (WithClear < Count) then
  begin
    Clear;
    FlushFromClear(Count);
  end else
    while (Count > 0) do
    begin
      OutputPlain(Prefix);
      dec(Count);
    end;
end;

procedure TRLEEncoder.FlushWithTable(Count: integer);
var
  RepeatMax		,
  RepeatLeft		,
  LeftOver		: integer;
begin
  RepeatMax := Count DIV RunLengthTableMax;
  LeftOver := Count MOD RunLengthTableMax;
  if (LeftOver <> 0) then
    RepeatLeft := 1
  else
    RepeatLeft := 0;

  if (OutCount + RepeatMax + RepeatLeft > MaxCodes) then
  begin
    RepeatMax := MaxCodes - OutCount;
    LeftOver := Count - (RepeatMax * RunLengthTableMax);
    RepeatLeft := 1 + ComputeTriangleCount(LeftOver, MaxCodes);
  end;

  if (1 + ComputeTriangleCount(Count, MaxCodes) < RepeatMax + RepeatLeft) then
  begin
    Clear;
    FlushFromClear(Count);
    exit;
  end;
  MaxOutClear;

  while (RepeatMax > 0) do
  begin
    OutputPlain(BaseCode + RunLengthTableMax-2);
    dec(RepeatMax);
  end;

  if (LeftOver > 0) then
  begin
    if (ClearFlag) then
      FlushFromClear(LeftOver)
    else if (LeftOver = 1) then
      OutputPlain(Prefix)
    else
      OutputPlain(BaseCode +  LeftOver - 2);
  end;
  ResetOutClear;
end;

procedure TRLEEncoder.Flush(RunLengthCount: integer);
begin
  if (RunLengthCount = 1) then
  begin
    OutputPlain(Prefix);
    exit;
  end;

  if (ClearFlag) then
    FlushFromClear(RunLengthCount)
  else if ((RunLengthTableMax < 2) or (RunLengthTablePixel <> Prefix)) then
    FlushClearOrRepeat(RunLengthCount)
  else
    FlushWithTable(RunLengthCount);
end;

procedure TRLEEncoder.DoCompress;
var
  Color			: CodeInt;
  RunLengthCount	: integer;

begin
  OutBumpInit := ClearCode - 1;

  // For images with a lot of runs, making OutClearInit larger will
  // give better compression.
  if (BitsPerPixel <= 3) then
    OutClearInit := 9
  else
    OutClearInit := OutBumpInit - 1;

  // max_ocodes = (1 << GIFBITS) - ((1 << (out_bits_init - 1)) + 3);
  // <=> MaxCodes := (1 SHL GIFCodeBits) - ((1 SHL (BitsPerCode - 1)) + 3);
  // <=> MaxCodes := (1 SHL GIFCodeBits) - ((1 SHL (InitialBitsPerCode - 1)) + 3);
  // <=> MaxCodes := (1 SHL GIFCodeBits) - (ClearCode + 3);
  // <=> MaxCodes := (1 SHL GIFCodeBits) - (EOFCode + 2);
  // <=> MaxCodes := (1 SHL GIFCodeBits) - (BaseCode + 1);
  // <=> MaxCodes := MaxCodesFromBits(GIFCodeBits) - BaseCode;
  MaxCodes := MaxCodesFromBits(GIFCodeBits) - BaseCode;

  Clear;
  RunLengthCount := 0;

  Pixel := Data;
  Prefix := -1; // Dummy value to make Color <> Prefix
  repeat
    // Fetch the next pixel
    Color := CodeInt(Pixel^);
    inc(Pixel);

    if (Color >= MaxColor) then
      Error(sInvalidColor);

    if (RunLengthCount > 0) and (Color <> Prefix) then
    begin
      // End of current run
      Flush(RunLengthCount);
      RunLengthCount := 0;
    end;

    if (Color = Prefix) then
      // Increment run length
      inc(RunLengthCount)
    else
    begin
      // Start new run
      Prefix := Color;
      RunLengthCount := 1;
    end;
  until not(BumpPixel);
  Flush(RunLengthCount);
end;

////////////////////////////////////////////////////////////////////////////////
//		TLZWEncoder - LZW encoder
////////////////////////////////////////////////////////////////////////////////
const
  TableMaxMaxCode	= (1 SHL GIFCodeBits);	//
  TableMaxFill		= TableMaxMaxCode-1;	// Clear table when it fills to
  						// this point.
  						// Note: Must be <= GIFCodeMax
type
  TLZWEncoder = class(TGIFEncoder)
  private
    Prefix		: CodeInt;	// Current run color
    FreeEntry		: CodeInt;	// next unused code in table
    HashTable		: THashTable;
  protected
    procedure Output(Value: integer); override;
    procedure Clear; override;
    procedure DoCompress; override;
  end;


procedure TLZWEncoder.Output(Value: integer);
begin
  inherited Output(Value);

  // If the next entry is going to be too big for the code size,
  // then increase it, if possible.
  if (FreeEntry > MaxCode) or (ClearFlag) then
  begin
    if (ClearFlag) then
    begin
      BitsPerCode := InitialBitsPerCode;
      MaxCode := MaxCodesFromBits(BitsPerCode);
      ClearFlag := False;
    end else
    begin
      inc(BitsPerCode);
      if (BitsPerCode = GIFCodeBits) then
        MaxCode := TableMaxMaxCode
      else
        MaxCode := MaxCodesFromBits(BitsPerCode);
    end;
  end;
end;

procedure TLZWEncoder.Clear;
begin
  inherited Clear;
  HashTable.Clear;
  FreeEntry := ClearCode + 2;
end;


procedure TLZWEncoder.DoCompress;
var
  Color			: char;
  NewKey		: KeyInt;
  NewCode		: CodeInt;

begin
  HashTable := THashTable.Create;
  try
    // clear hash table and sync decoder
    Clear;

    Pixel := Data;
    Prefix := CodeInt(Pixel^);
    inc(Pixel);
    if (Prefix >= MaxColor) then
      Error(sInvalidColor);
    while (BumpPixel) do
    begin
      // Fetch the next pixel
      Color := Pixel^;
      inc(Pixel);
      if (ord(Color) >= MaxColor) then
        Error(sInvalidColor);

      // Append Postfix to Prefix and lookup in table...
      NewKey := (KeyInt(Prefix) SHL 8) OR ord(Color);
      NewCode := HashTable.Lookup(NewKey);
      if (NewCode >= 0) then
      begin
        // ...if found, get next pixel
        Prefix := NewCode;
        continue;
      end;

      // ...if not found, output and start over
      Output(Prefix);
      Prefix := CodeInt(Color);

      if (FreeEntry < TableMaxFill) then
      begin
        HashTable.Insert(NewKey, FreeEntry);
        inc(FreeEntry);
      end else
        Clear;
    end;
    Output(Prefix);
  finally
    HashTable.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFSubImage
//
////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////
//		TGIFSubImage.Compress
/////////////////////////////////////////////////////////////////////////
procedure TGIFSubImage.Compress(Stream: TStream);
var
  Encoder		: TGIFEncoder;
  BitsPerPixel		: BYTE;
  MaxColors		: integer;
begin
  if (ColorMap.Count > 0) then
  begin
    MaxColors := ColorMap.Count;
    BitsPerPixel := ColorMap.BitsPerPixel
  end else
  begin
    BitsPerPixel := Image.BitsPerPixel;
    MaxColors := 1 SHL BitsPerPixel;
  end;

  // Create a RLE or LZW GIF encoder
  if (Image.Compression = gcRLE) then
    Encoder := TRLEEncoder.Create
  else
    Encoder := TLZWEncoder.Create;
  try
    Encoder.Warning := Image.Warning;
    Encoder.Compress(Stream, BitsPerPixel, Width, Height, Interlaced, FData, MaxColors);
  finally
    Encoder.Free;
  end;
end;

function TGIFExtensionList.GetExtension(Index: Integer): TGIFExtension;
begin
  Result := TGIFExtension(Items[Index]);
end;

procedure TGIFExtensionList.SetExtension(Index: Integer; Extension: TGIFExtension);
begin
  Items[Index] := Extension;
end;

procedure TGIFExtensionList.LoadFromStream(Stream: TStream; Parent: TObject);
var
  b			: BYTE;
  Extension		: TGIFExtension;
  ExtensionClass	: TGIFExtensionClass;
begin
  // Peek ahead to determine block type
  if (Stream.Read(b, 1) <> 1) then
    exit;
  while not(b in [bsTrailer, bsImageDescriptor]) do
  begin
    if (b = bsExtensionIntroducer) then
    begin
      ExtensionClass := TGIFExtension.FindExtension(Stream);
      if (ExtensionClass = nil) then
        Error(sUnknownExtension);
      Stream.Seek(-1, soFromCurrent);
      Extension := ExtensionClass.Create(Parent as TGIFSubImage);
      try
        Extension.LoadFromStream(Stream);
        Add(Extension);
      except
        Extension.Free;
        raise;
      end;
    end else
    begin
      Warning(gsWarning, sBadExtensionLabel);
      break;
    end;
    if (Stream.Read(b, 1) <> 1) then
      exit;
  end;
  Stream.Seek(-1, soFromCurrent);
end;

const
  { image descriptor bit masks }
  idLocalColorTable	= $80;    { set if a local color table follows }
  idInterlaced		= $40;    { set if image is interlaced }
  idSort		= $20;    { set if color table is sorted }
  idReserved		= $0C;    { reserved - must be set to $00 }
  idColorTableSize	= $07;    { size of color table as above }

constructor TGIFSubImage.Create(GIFImage: TGIFImage);
begin
  inherited Create(GIFImage);
  FExtensions := TGIFExtensionList.Create(GIFImage);
  FColorMap := TGIFLocalColorMap.Create(self);
  FImageDescriptor.Separator := bsImageDescriptor;
  FImageDescriptor.Left := 0;
  FImageDescriptor.Top := 0;
  FImageDescriptor.Width := 0;
  FImageDescriptor.Height := 0;
  FImageDescriptor.PackedFields := 0;
  FBitmap := nil;
  FMask := 0;
  FNeedMask := True;
  FData := nil;
  FDataSize := 0;
  FTransparent := False;
  FGCE := nil;
  // Remember to synchronize with TGIFSubImage.Clear
end;

destructor TGIFSubImage.Destroy;
begin
  if (FGIFImage <> nil) then
    FGIFImage.Images.Remove(self);
  Clear;
  FExtensions.Free;
  FColorMap.Free;
  if (FLocalPalette <> 0) then
    DeleteObject(FLocalPalette);
  inherited Destroy;
end;

procedure TGIFSubImage.Clear;
begin
  FExtensions.Clear;
  FColorMap.Clear;
  FreeImage;
  Height := 0;
  Width := 0;
  FTransparent := False;
  FGCE := nil;
  FreeBitmap;
  FreeMask;
  // Remember to synchronize with TGIFSubImage.Create
end;

function TGIFSubImage.GetEmpty: Boolean;
begin
  Result := ((FData = nil) or (FDataSize = 0) or (Height = 0) or (Width = 0));
end;

function TGIFSubImage.GetPalette: HPALETTE;
begin
  if (FBitmap <> nil) and (FBitmap.Palette <> 0) then
    // Use bitmaps own palette if possible
    Result := FBitmap.Palette
  else if (FLocalPalette <> 0) then
    // Or a previously exported local palette
    Result := FLocalPalette
  else if (Image.DoDither) then
  begin
    // or create a new dither palette
    FLocalPalette := WebPalette;
    Result := FLocalPalette;
  end
  else if (ColorMap.Count > 0) then
  begin
    // or create a new if first time
    FLocalPalette := ColorMap.ExportPalette;
    Result := FLocalPalette;
  end else
    // Use global palette if everything else fails
    Result := Image.Palette;
end;

procedure TGIFSubImage.SetPalette(Value: HPalette);
var
  NeedNewBitmap		: boolean;
begin
  if (Value <> FLocalPalette) then
  begin
    // Zap old palette
    if (FLocalPalette <> 0) then
      DeleteObject(FLocalPalette);
    // Zap bitmap unless new palette is same as bitmaps own
    NeedNewBitmap := (FBitmap <> nil) and (Value <> FBitmap.Palette);

    // Use new palette
    FLocalPalette := Value;
    if (NeedNewBitmap) then
    begin
      // Need to create new bitmap and repaint
      FreeBitmap;
      Image.PaletteModified := True;
      Image.Changed(Self);
    end;
  end;
end;

procedure TGIFSubImage.NeedImage;
begin
  if (FData = nil) then
    NewImage;
  if (FDataSize = 0) then
    Error(sEmptyImage);
end;

procedure TGIFSubImage.NewImage;
var
  NewSize		: longInt;
begin
  FreeImage;
  NewSize := Height * Width;
  if (NewSize <> 0) then
  begin
    GetMem(FData, NewSize);
    FillChar(FData^, NewSize, 0);
  end else
    FData := nil;
  FDataSize := NewSize;
end;

procedure TGIFSubImage.FreeImage;
begin
  if (FData <> nil) then
    FreeMem(FData);
  FDataSize := 0;
  FData := nil;
end;

function TGIFSubImage.GetHasBitmap: boolean;
begin
  Result := (FBitmap <> nil);
end;

procedure TGIFSubImage.SetHasBitmap(Value: boolean);
begin
  if (Value <> (FBitmap <> nil)) then
  begin
    if (Value) then
      Bitmap // Referencing Bitmap will automatically create it
    else
      FreeBitmap;
  end;
end;

procedure TGIFSubImage.NewBitmap;
begin
  FreeBitmap;
  FBitmap := TBitmap.Create;
end;

procedure TGIFSubImage.FreeBitmap;
begin
  if (FBitmap <> nil) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure TGIFSubImage.FreeMask;
begin
  if (FMask <> 0) then
  begin
    DeleteObject(FMask);
    FMask := 0;
  end;
  FNeedMask := True;
end;

function TGIFSubImage.HasMask: boolean;
begin
  if (FNeedMask) and (Transparent) then
  begin
    // Zap old bitmap
    FreeBitmap;
    // Create new bitmap and mask
    GetBitmap;
  end;
  Result := (FMask <> 0);
end;

function TGIFSubImage.GetBounds(Index: integer): WORD;
begin
  case (Index) of
    1: Result := FImageDescriptor.Left;
    2: Result := FImageDescriptor.Top;
    3: Result := FImageDescriptor.Width;
    4: Result := FImageDescriptor.Height;
  else
    Result := 0; // To avoid compiler warnings
  end;
end;

procedure TGIFSubImage.SetBounds(Index: integer; Value: WORD);
begin
  case (Index) of
    1: DoSetBounds(Value, FImageDescriptor.Top, FImageDescriptor.Width, FImageDescriptor.Height);
    2: DoSetBounds(FImageDescriptor.Left, Value, FImageDescriptor.Width, FImageDescriptor.Height);
    3: DoSetBounds(FImageDescriptor.Left, FImageDescriptor.Top, Value, FImageDescriptor.Height);
    4: DoSetBounds(FImageDescriptor.Left, FImageDescriptor.Top, FImageDescriptor.Width, Value);
  end;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TGIFSubImage.DoGetDitherBitmap: TBitmap;
var
  ColorLookup		: TColorLookup;
  Ditherer		: TDitherEngine;
  DIBResult		: TDIB;
  Src			: PChar;
  Dst			: PChar;

  Row			: integer;
  Color			: TGIFColor;
  ColMap		: PColorMap;
  Index			: byte;
  TransparentIndex	: byte;
  IsTransparent		: boolean;
  WasTransparent	: boolean;
  MappedTransparentIndex: char;

  MaskBits		: PChar;
  MaskDest		: PChar;
  MaskRow		: PChar;
  MaskRowWidth		,
  MaskRowBitWidth	: integer;
  Bit			,
  RightBit		: BYTE;

begin
  Result := TBitmap.Create;
  try

{$IFNDEF VER9x}
    if (Width*Height > BitmapAllocationThreshold) then
      SetPixelFormat(Result, pf1bit); // To reduce resource consumption of resize
{$ENDIF}

    if (Empty) then
    begin
      // Set bitmap width and height
      Result.Width := Width;
      Result.Height := Height;

      // Build and copy palette to bitmap
      Result.Palette := CopyPalette(Palette);

      exit;
    end;

    ColorLookup := nil;
    Ditherer := nil;
    DIBResult := nil;
    try // Protect above resources
      ColorLookup := TNetscapeColorLookup.Create(Palette);
      Ditherer := TFloydSteinbergDitherer.Create(Width, ColorLookup);
      // Get DIB buffer for scanline operations
      // It is assumed that the source palette is the 216 color Netscape palette
      DIBResult := TDIBWriter.Create(Result, pf8bit, Width, Height, Palette);

      // Determine if this image is transparent
      ColMap := ActiveColorMap.Data;
      IsTransparent := FNeedMask and Transparent;
      WasTransparent := False;
      FNeedMask := False;
      TransparentIndex := 0;
      MappedTransparentIndex := #0;
      if (FMask = 0) and (IsTransparent) then
      begin
        IsTransparent := True;
        TransparentIndex := GraphicControlExtension.TransparentColorIndex;
        Color := ColMap[ord(TransparentIndex)];
        MappedTransparentIndex := char(Color.Blue DIV 51 +
          MulDiv(6, Color.Green, 51) + MulDiv(36, Color.Red, 51)+1);
      end;

      // Allocate bit buffer for transparency mask
      MaskDest := nil;
      Bit := $00;
      if (IsTransparent) then
      begin
        MaskRowWidth := ((Width+15) DIV 16) * 2;
        MaskRowBitWidth := (Width+7) DIV 8;
        RightBit := $01 SHL ((8 - (Width AND $0007)) AND $0007);
        GetMem(MaskBits, MaskRowWidth * Height);
        FillChar(MaskBits^, MaskRowWidth * Height, 0);
      end else
      begin
        MaskBits := nil;
        MaskRowWidth := 0;
        MaskRowBitWidth := 0;
        RightBit := $00;
      end;

      try
        // Process the image
        Row := 0;
        MaskRow := MaskBits;
        Src := FData;
        while (Row < Height) do
        begin
          if ((Row AND $1F) = 0) then
            Image.Progress(Self, psRunning, MulDiv(Row, 100, Height),
              False, Rect(0,0,0,0), sProgressRendering);

          Dst := DIBResult.ScanLine[Row];
          if (IsTransparent) then
          begin
            // Preset all pixels to transparent
            FillChar(Dst^, Width, ord(MappedTransparentIndex));
            if (Ditherer.Direction = 1) then
            begin
              MaskDest := MaskRow;
              Bit := $80;
            end else
            begin
              MaskDest := MaskRow + MaskRowBitWidth-1;
              Bit := RightBit;
            end;
          end;
          inc(Dst, Ditherer.Column);

          while (Ditherer.Column < Ditherer.Width) and (Ditherer.Column >= 0) do
          begin
            Index := ord(Src^);
            Color := ColMap[ord(Index)];

            if (IsTransparent) and (Index = TransparentIndex) then
            begin
              MaskDest^ := char(byte(MaskDest^) OR Bit);
              WasTransparent := True;
              Ditherer.NextColumn;
            end else
            begin
              // Dither and map a single pixel
              Dst^ := Ditherer.Dither(Color.Red, Color.Green, Color.Blue,
                Color.Red, Color.Green, Color.Blue);
            end;

            if (IsTransparent) then
            begin
              if (Ditherer.Direction = 1) then
              begin
                Bit := Bit SHR 1;
                if (Bit = $00) then
                begin
                  Bit := $80;
                  inc(MaskDest, 1);
                end;
              end else
              begin
                Bit := Bit SHL 1;
                if (Bit = $00) then
                begin
                  Bit := $01;
                  dec(MaskDest, 1);
                end;
              end;
            end;

            inc(Src, Ditherer.Direction);
            inc(Dst, Ditherer.Direction);
          end;

          if (IsTransparent) then
            Inc(MaskRow, MaskRowWidth);
          Inc(Row);
          inc(Src, Width-Ditherer.Direction);
          Ditherer.NextLine;
        end;

        // Transparent paint needs a mask bitmap
        if (IsTransparent) and (WasTransparent) then
          FMask := CreateBitmap(Width, Height, 1, 1, MaskBits);
      finally
        if (MaskBits <> nil) then
          FreeMem(MaskBits);
      end;
    finally
      if (ColorLookup <> nil) then
        ColorLookup.Free;
      if (Ditherer <> nil) then
        Ditherer.Free;
      if (DIBResult <> nil) then
        DIBResult.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

function TGIFSubImage.DoGetBitmap: TBitmap;
var
  ScanLineRow		: Integer;
  DIBResult		: TDIB;
  DestScanLine		,
  Src			: PChar;
  TransparentIndex	: byte;
  IsTransparent		: boolean;
  WasTransparent	: boolean;

  MaskBits		: PChar;
  MaskDest		: PChar;
  MaskRow		: PChar;
  MaskRowWidth		: integer;
  Col			: integer;
  MaskByte		: byte;
  Bit			: byte;
begin
  Result := TBitmap.Create;
  try

{$IFNDEF VER9x}
    if (Width*Height > BitmapAllocationThreshold) then
      SetPixelFormat(Result, pf1bit); // To reduce resource consumption of resize
{$ENDIF}

    if (Empty) then
    begin
      // Set bitmap width and height
      Result.Width := Width;
      Result.Height := Height;

      // Build and copy palette to bitmap
      Result.Palette := CopyPalette(Palette);

      exit;
    end;

    // Get DIB buffer for scanline operations
    DIBResult := TDIBWriter.Create(Result, pf8bit, Width, Height, Palette);
    try

      // Determine if this image is transparent
      IsTransparent := FNeedMask and Transparent;
      WasTransparent := False;
      FNeedMask := False;
      TransparentIndex := 0;
      if (FMask = 0) and (IsTransparent) then
      begin
        IsTransparent := True;
        TransparentIndex := GraphicControlExtension.TransparentColorIndex;
      end;
      // Allocate bit buffer for transparency mask
      if (IsTransparent) then
      begin
        MaskRowWidth := ((Width+15) DIV 16) * 2;
        GetMem(MaskBits, MaskRowWidth * Height);
        FillChar(MaskBits^, MaskRowWidth * Height, 0);
        IsTransparent := (MaskBits <> nil);
      end else
      begin
        MaskBits := nil;
        MaskRowWidth := 0;
      end;

      try
        ScanLineRow := 0;
        Src := FData;
        MaskRow := MaskBits;
        while (ScanLineRow < Height) do
        begin
          DestScanline := DIBResult.ScanLine[ScanLineRow];

          if ((ScanLineRow AND $1F) = 0) then
            Image.Progress(Self, psRunning, MulDiv(ScanLineRow, 100, Height),
              False, Rect(0,0,0,0), sProgressRendering);

          Move(Src^, DestScanline^, Width);
          Inc(ScanLineRow);

          if (IsTransparent) then
          begin
            Bit := $80;
            MaskDest := MaskRow;
            MaskByte := 0;
            for Col := 0 to Width-1 do
            begin
              // Set a bit in the mask if the pixel is transparent
              if (Src^ = char(TransparentIndex)) then
                MaskByte := MaskByte OR Bit;

              Bit := Bit SHR 1;
              if (Bit = $00) then
              begin
                // Store a mask byte for each 8 pixels
                Bit := $80;
                WasTransparent := WasTransparent or (MaskByte <> 0);
                MaskDest^ := char(MaskByte);
                inc(MaskDest);
                MaskByte := 0;
              end;
              Inc(Src);
            end;
            // Save the last mask byte in case the width isn't divisable by 8
            if (MaskByte <> 0) then
            begin
              WasTransparent := True;
              MaskDest^ := char(MaskByte);
            end;
            Inc(MaskRow, MaskRowWidth);
          end else
            Inc(Src, Width);
        end;

        // Transparent paint needs a mask bitmap
        if (IsTransparent) and (WasTransparent) then
          FMask := CreateBitmap(Width, Height, 1, 1, MaskBits);
      finally
        if (MaskBits <> nil) then
          FreeMem(MaskBits);
      end;
    finally
      // Free DIB buffer used for scanline operations
      DIBResult.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

{$ifdef DEBUG_RENDERPERFORMANCE}
var
  ImageCount		: DWORD = 0;
  RenderTime		: DWORD = 0;
{$endif}
function TGIFSubImage.GetBitmap: TBitmap;
var
  n			: integer;
{$ifdef DEBUG_RENDERPERFORMANCE}
  RenderStartTime	: DWORD;
{$endif}
begin
{$ifdef DEBUG_RENDERPERFORMANCE}
  if (GetAsyncKeyState(VK_CONTROL) <> 0) then
  begin
    ShowMessage(format('Render %d images in %d mS, Rate %d mS/image (%d images/S)',
      [ImageCount, RenderTime,
       RenderTime DIV (ImageCount+1),
       MulDiv(ImageCount, 1000, RenderTime+1)]));
  end;
{$endif}
  Result := FBitmap;
  if (Result <> nil) or (Empty) then
    Exit;

{$ifdef DEBUG_RENDERPERFORMANCE}
  inc(ImageCount);
  RenderStartTime := timeGetTime;
{$endif}
  try
    Image.Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressRendering);
    try

      if (Image.DoDither) then
        // Create dithered bitmap
        FBitmap := DoGetDitherBitmap
      else
        // Create "regular" bitmap
        FBitmap := DoGetBitmap;

      Result := FBitmap;

    finally
      if ExceptObject = nil then
        n := 100
      else
        n := 0;
      Image.Progress(Self, psEnding, n, Image.PaletteModified, Rect(0,0,0,0),
        sProgressRendering);
      // Make sure new palette gets realized, in case OnProgress event didn't.
      if Image.PaletteModified then
        Image.Changed(Self);
    end;
  except
    on EAbort do ;   // OnProgress can raise EAbort to cancel image load
  end;
{$ifdef DEBUG_RENDERPERFORMANCE}
  inc(RenderTime, timeGetTime-RenderStartTime);
{$endif}
end;

procedure TGIFSubImage.SetBitmap(Value: TBitmap);
begin
  FreeBitmap;
  if (Value <> nil) then
    Assign(Value);
end;

function TGIFSubImage.GetActiveColorMap: TGIFColorMap;
begin
  if (ColorMap.Count > 0) or (Image.GlobalColorMap.Count = 0) then
    Result := ColorMap
  else
    Result := Image.GlobalColorMap;
end;

function TGIFSubImage.GetInterlaced: boolean;
begin
  Result := (FImageDescriptor.PackedFields AND idInterlaced) <> 0;
end;

procedure TGIFSubImage.SetInterlaced(Value: boolean);
begin
  if (Value) then
    FImageDescriptor.PackedFields := FImageDescriptor.PackedFields OR idInterlaced
  else
    FImageDescriptor.PackedFields := FImageDescriptor.PackedFields AND NOT(idInterlaced);
end;

function TGIFSubImage.GetVersion: TGIFVersion;
var
  v			: TGIFVersion;
  i			: integer;
begin
  if (ColorMap.Optimized) then
    Result := gv89a
  else
    Result := inherited GetVersion;
  i := 0;
  while (Result < high(TGIFVersion)) and (i < FExtensions.Count) do
  begin
    v := FExtensions[i].Version;
    if (v > Result) then
      Result := v;
  end;
end;

function TGIFSubImage.GetColorResolution: integer;
begin
  Result := ColorMap.BitsPerPixel-1;
end;

function TGIFSubImage.GetBitsPerPixel: integer;
begin
  Result := ColorMap.BitsPerPixel;
end;

function TGIFSubImage.GetBoundsRect: TRect;
begin
  Result := Rect(FImageDescriptor.Left,
    FImageDescriptor.Top,
    FImageDescriptor.Left+FImageDescriptor.Width,
    FImageDescriptor.Top+FImageDescriptor.Height);
end;

procedure TGIFSubImage.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
var
  TooLarge		: boolean;
  Zap			: boolean;
begin
  Zap := (FImageDescriptor.Width <> Width) or (FImageDescriptor.Height <> AHeight);
  FImageDescriptor.Left := ALeft;
  FImageDescriptor.Top := ATop;
  FImageDescriptor.Width := AWidth;
  FImageDescriptor.Height := AHeight;

  // Delete existing image and bitmaps if size has changed
  if (Zap) then
  begin
    FreeBitmap;
    FreeMask;
    FreeImage;
    // ...and allocate a new image
    NewImage;
  end;

  TooLarge := False;
  // Set width & height if added image is larger than existing images
{$IFDEF STRICT_MOZILLA}
  // From Mozilla source:
  // Work around broken GIF files where the logical screen
  // size has weird width or height. [...]
  if (Image.Width < AWidth) or (Image.Height < AHeight) then
  begin
    TooLarge := True;
    Image.Width := AWidth;
    Image.Height := AHeight;
    Left := 0;
    Top := 0;
  end;
{$ELSE}
  if (Image.Width < ALeft+AWidth) then
  begin
    if (Image.Width > 0) then
    begin
      TooLarge := True;
      Warning(gsWarning, sBadWidth)
    end;
    Image.Width := ALeft+AWidth;
  end;
  if (Image.Height < ATop+AHeight) then
  begin
    if (Image.Height > 0) then
    begin
      TooLarge := True;
      Warning(gsWarning, sBadHeight)
    end;
    Image.Height := ATop+AHeight;
  end;
{$ENDIF}

  if (TooLarge) then
    Warning(gsWarning, sScreenSizeExceeded);
end;

procedure TGIFSubImage.SetBoundsRect(const Value: TRect);
begin
  DoSetBounds(Value.Left, Value.Top, Value.Right-Value.Left+1, Value.Bottom-Value.Top+1);
end;

function TGIFSubImage.GetClientRect: TRect;
begin
  Result := Rect(0, 0, FImageDescriptor.Width, FImageDescriptor.Height);
end;

function TGIFSubImage.GetPixel(x, y: integer): BYTE;
begin
  if (x < 0) or (x > Width-1) then
    Error(sBadPixelCoordinates);
  Result := BYTE(PChar(longInt(Scanline[y]) + x)^);
end;

function TGIFSubImage.GetScanline(y: integer): pointer;
begin
  if (y < 0) or (y > Height-1) then
    Error(sBadPixelCoordinates);
  NeedImage;
  Result := pointer(longInt(FData) + y * Width);
end;

procedure TGIFSubImage.Prepare;
var
  Pack			: BYTE;
begin
  Pack := FImageDescriptor.PackedFields;
  if (ColorMap.Count > 0) then
  begin
    Pack := idLocalColorTable;
    if (ColorMap.Optimized) then
      Pack := Pack OR idSort;
    Pack := (Pack AND NOT(idColorTableSize)) OR (ColorResolution AND idColorTableSize);
  end else
    Pack := Pack AND NOT(idLocalColorTable OR idSort OR idColorTableSize);
  FImageDescriptor.PackedFields := Pack;
end;

procedure TGIFSubImage.SaveToStream(Stream: TStream);
begin
  FExtensions.SaveToStream(Stream);
  if (Empty) then
    exit;
  Prepare;
  Stream.Write(FImageDescriptor, sizeof(TImageDescriptor));
  ColorMap.SaveToStream(Stream);
  Compress(Stream);
end;

procedure TGIFSubImage.LoadFromStream(Stream: TStream);
var
  ColorCount		: integer;
  b			: BYTE;
begin
  Clear;
  FExtensions.LoadFromStream(Stream, self);
  // Check for extension without image
  if (Stream.Read(b, 1) <> 1) then
    exit;
  Stream.Seek(-1, soFromCurrent);
  if (b = bsTrailer) or (b = 0) then
    exit;

  ReadCheck(Stream, FImageDescriptor, sizeof(TImageDescriptor));

  // From Mozilla source:
  // Work around more broken GIF files that have zero image
  // width or height
  if (FImageDescriptor.Height = 0) or (FImageDescriptor.Width = 0) then
  begin
    FImageDescriptor.Height := Image.Height;
    FImageDescriptor.Width := Image.Width;
    Warning(gsWarning, sScreenSizeExceeded);
  end;

  if (FImageDescriptor.PackedFields AND idLocalColorTable = idLocalColorTable) then
  begin
    ColorCount := 2 SHL (FImageDescriptor.PackedFields AND idColorTableSize);
    if (ColorCount < 2) or (ColorCount > 256) then
      Error(sImageBadColorSize);
    ColorMap.LoadFromStream(Stream, ColorCount);
  end;

  Decompress(Stream);

  // On-load rendering
  if (GIFImageRenderOnLoad) then
    // Touch bitmap to force frame to be rendered
    Bitmap;
end;

procedure TGIFSubImage.AssignTo(Dest: TPersistent);
begin
  if (Dest is TBitmap) then
    Dest.Assign(Bitmap)
  else
    inherited AssignTo(Dest);
end;

procedure TGIFSubImage.Assign(Source: TPersistent);
var
  MemoryStream		: TMemoryStream;
  i			: integer;
  PixelFormat		: TPixelFormat;
  DIBSource		: TDIB;
  ABitmap		: TBitmap;

  procedure Import8Bit(Dest: PChar);
  var
    y			: integer;
  begin
    // Copy colormap
{$ifdef VER10_PLUS}
    if (FBitmap.HandleType = bmDIB) then
      FColorMap.ImportDIBColors(FBitmap.Canvas.Handle)
    else
{$ENDIF}
      FColorMap.ImportPalette(FBitmap.Palette);
    // Copy pixels
    for y := 0 to Height-1 do
    begin
      if ((y AND $1F) = 0) then
        Image.Progress(Self, psRunning, MulDiv(y, 100, Height), False, Rect(0,0,0,0), sProgressConverting);
      Move(DIBSource.Scanline[y]^, Dest^, Width);
      inc(Dest, Width);
    end;
  end;

  procedure Import4Bit(Dest: PChar);
  var
    x, y		: integer;
    Scanline		: PChar;
  begin
    // Copy colormap
    FColorMap.ImportPalette(FBitmap.Palette);
    // Copy pixels
    for y := 0 to Height-1 do
    begin
      if ((y AND $1F) = 0) then
        Image.Progress(Self, psRunning, MulDiv(y, 100, Height), False, Rect(0,0,0,0), sProgressConverting);
      ScanLine := DIBSource.Scanline[y];
      for x := 0 to Width-1 do
      begin
        if (x AND $01 = 0) then
          Dest^ := chr(ord(ScanLine^) SHR 4)
        else
        begin
          Dest^ := chr(ord(ScanLine^) AND $0F);
          inc(ScanLine);
        end;
        inc(Dest);
      end;
    end;
  end;

  procedure Import1Bit(Dest: PChar);
  var
    x, y		: integer;
    Scanline		: PChar;
    Bit			: integer;
    Byte		: integer;
  begin
    // Copy colormap
    FColorMap.ImportPalette(FBitmap.Palette);
    // Copy pixels
    for y := 0 to Height-1 do
    begin
      if ((y AND $1F) = 0) then
        Image.Progress(Self, psRunning, MulDiv(y, 100, Height), False, Rect(0,0,0,0), sProgressConverting);
      ScanLine := DIBSource.Scanline[y];
      x := Width;
      Bit := 0;
      Byte := 0; // To avoid compiler warning
      while (x > 0) do
      begin
        if (Bit = 0) then
        begin
          Bit := 8;
          Byte := ord(ScanLine^);
          inc(Scanline);
        end;
        Dest^ := chr((Byte AND $80) SHR 7);
        Byte := Byte SHL 1;
        inc(Dest);
        dec(Bit);
        dec(x);
      end;
    end;
  end;

  procedure Import24Bit(Dest: PChar);
  type
    TCacheEntry = record
      Color		: TColor;
      Index		: integer;
    end;
  const
    // Size of palette cache. Must be 2^n.
    // The cache holds the palette index of the last "CacheSize" colors
    // processed. Hopefully the cache can speed things up a bit... Initial
    // testing shows that this is indeed the case at least for non-dithered
    // bitmaps.
    // All the same, a small hash table would probably be much better.
    CacheSize		= 8;
  var
    i			: integer;
    Cache		: array[0..CacheSize-1] of TCacheEntry;
    LastEntry		: integer;
    Scanline		: PRGBTriple;
    Pixel		: TColor;
    RGBTriple		: TRGBTriple absolute Pixel;
    x, y		: integer;
    ColorMap		: PColorMap;
    t			: byte;
  label
    NextPixel;
  begin
    for i := 0 to CacheSize-1 do
      Cache[i].Index := -1;
    LastEntry := 0;

    // Copy all pixels and build colormap
    for y := 0 to Height-1 do
    begin
      if ((y AND $1F) = 0) then
        Image.Progress(Self, psRunning, MulDiv(y, 100, Height), False, Rect(0,0,0,0), sProgressConverting);
      ScanLine := DIBSource.Scanline[y];
      for x := 0 to Width-1 do
      begin
        Pixel := 0;
        RGBTriple := Scanline^;
        // Scan cache for color from most recently processed color to last
        // recently processed. This is done because TColorMap.AddUnique is very slow.
        i := LastEntry;
        repeat
          if (Cache[i].Index = -1) then
            break;
          if (Cache[i].Color = Pixel) then
          begin
            Dest^ := chr(Cache[i].Index);
            LastEntry := i;
            goto NextPixel;
          end;
          if (i = 0) then
            i := CacheSize-1
          else
            dec(i);
        until (i = LastEntry);
        // Color not found in cache, do it the slow way instead
        Dest^ := chr(FColorMap.AddUnique(Pixel));
        // Add color and index to cache
        LastEntry := (LastEntry + 1) AND (CacheSize-1);
        Cache[LastEntry].Color := Pixel;
        Cache[LastEntry].Index := ord(Dest^);

        NextPixel:
        Inc(Dest);
        Inc(Scanline);
      end;
    end;
    // Convert colors in colormap from BGR to RGB
    ColorMap := FColorMap.Data;
    i := FColorMap.Count;
    while (i > 0) do
    begin
      t := ColorMap^[0].Red;
      ColorMap^[0].Red := ColorMap^[0].Blue;
      ColorMap^[0].Blue := t;
      inc(integer(ColorMap), sizeof(TGIFColor));
      dec(i);
    end;
  end;

  procedure ImportViaDraw(ABitmap: TBitmap; Graphic: TGraphic);
  begin
    ABitmap.Height := Graphic.Height;
    ABitmap.Width := Graphic.Width;

    // Note: Disable the call to SafeSetPixelFormat below to import
    // in max number of colors with the risk of having to use
    // TCanvas.Pixels to do it (very slow).

    // Make things a little easier for TGIFSubImage.Assign by converting
    // pfDevice to a more import friendly format
{$ifdef SLOW_BUT_SAFE}
    SafeSetPixelFormat(ABitmap, pf8bit);
{$else}
{$ifndef VER9x}
    SetPixelFormat(ABitmap, pf24bit);
{$endif}
{$endif}
    ABitmap.Canvas.Draw(0, 0, Graphic);
  end;

  procedure AddMask(Mask: TBitmap);
  var
    DIBReader		: TDIBReader;
    TransparentIndex	: integer;
    i			,
    j			: integer;
    GIFPixel		,
    MaskPixel		: PChar;
    WasTransparent	: boolean;
    GCE			: TGIFGraphicControlExtension;
  begin
    // Optimize colormap to make room for transparent color
    ColorMap.Optimize;
    // Can't make transparent if no color or colormap full
    if (ColorMap.Count = 0) or (ColorMap.Count = 256) then
      exit;

    // Add the transparent color to the color map
    TransparentIndex := ColorMap.Add(TColor(0));
    WasTransparent := False;

    DIBReader := TDIBReader.Create(Mask, pf8bit);
    try
      for i := 0 to Height-1 do
      begin
        MaskPixel := DIBReader.Scanline[i];
        GIFPixel := Scanline[i];
        for j := 0 to Width-1 do
        begin
          // Change all unmasked pixels to transparent
          if (MaskPixel^ <> #0) then
          begin
            GIFPixel^ := chr(TransparentIndex);
            WasTransparent := True;
          end;
          inc(MaskPixel);
          inc(GIFPixel);
        end;
      end;
    finally
      DIBReader.Free;
    end;

    // Add a Graphic Control Extension if any part of the mask was transparent
    if (WasTransparent) then
    begin
      GCE := TGIFGraphicControlExtension.Create(self);
      GCE.Transparent := True;
      GCE.TransparentColorIndex := TransparentIndex;
      Extensions.Add(GCE);
    end else
      // Otherwise removed the transparency color since it wasn't used
      ColorMap.Delete(TransparentIndex);
  end;

  procedure AddMaskOnly(hMask: hBitmap);
  var
    Mask		: TBitmap;
  begin
    if (hMask = 0) then
      exit;

    // Encapsulate the mask
    Mask := TBitmap.Create;
    try
      Mask.Handle := hMask;
      AddMask(Mask);
    finally
      Mask.ReleaseHandle;
      Mask.Free;
    end;
  end;

  procedure AddIconMask(Icon: TIcon);
  var
    IconInfo		: TIconInfo;
  begin
    if (not GetIconInfo(Icon.Handle, IconInfo)) then
      exit;

    // Extract the icon mask
    AddMaskOnly(IconInfo.hbmMask);
  end;

  procedure AddMetafileMask(Metafile: TMetaFile);
  var
    Mask1		,
    Mask2		: TBitmap;

    procedure DrawMetafile(ABitmap: TBitmap; Background: TColor);
    begin
      ABitmap.Width := Metafile.Width;
      ABitmap.Height := Metafile.Height;
{$ifndef VER9x}
      SetPixelFormat(ABitmap, pf24bit);
{$endif}
      ABitmap.Canvas.Brush.Color := Background;
      ABitmap.Canvas.Brush.Style := bsSolid;
      ABitmap.Canvas.FillRect(ABitmap.Canvas.ClipRect);
      ABitmap.Canvas.Draw(0,0, Metafile);
    end;

  begin
    // Create the metafile mask
    Mask1 := TBitmap.Create;
    try
      Mask2 := TBitmap.Create;
      try
        DrawMetafile(Mask1, clWhite);
        DrawMetafile(Mask2, clBlack);
        Mask1.Canvas.CopyMode := cmSrcInvert;
        Mask1.Canvas.Draw(0,0, Mask2);
        AddMask(Mask1);
      finally
        Mask2.Free;
      end;
    finally
      Mask1.Free;
    end;
  end;

begin
  if (Source = self) then
    exit;
  if (Source = nil) then
  begin
    Clear;
  end else
  //
  // TGIFSubImage import
  //
  if (Source is TGIFSubImage) then
  begin
    // Zap existing colormap, extensions and bitmap
    Clear;
    if (TGIFSubImage(Source).Empty) then
      exit;
    // Copy source data
    FImageDescriptor := TGIFSubImage(Source).FImageDescriptor;
    FTransparent := TGIFSubImage(Source).Transparent;
    // Copy image data
    NewImage;
    if (FData <> nil) and (TGIFSubImage(Source).Data <> nil) then
      Move(TGIFSubImage(Source).Data^, FData^, FDataSize);
    // Copy palette
    FColorMap.Assign(TGIFSubImage(Source).ColorMap);
    // Copy extensions
    if (TGIFSubImage(Source).Extensions.Count > 0) then
    begin
      MemoryStream := TMemoryStream.Create;
      try
        TGIFSubImage(Source).Extensions.SaveToStream(MemoryStream);
        MemoryStream.Seek(0, soFromBeginning);
        Extensions.LoadFromStream(MemoryStream, Self);
      finally
        MemoryStream.Free;
      end;
    end;

    // Copy bitmap representation
    // (Not really nescessary but improves performance if the bitmap is needed
    // later on)
    if (TGIFSubImage(Source).HasBitmap) then
    begin
      NewBitmap;
      FBitmap.Assign(TGIFSubImage(Source).Bitmap);
    end;
  end else
  //
  // Bitmap import
  //
  if (Source is TBitmap) then
  begin
    // Zap existing colormap, extensions and bitmap
    Clear;
    if (TBitmap(Source).Empty) then
      exit;

    Width := TBitmap(Source).Width;
    Height := TBitmap(Source).Height;

    PixelFormat := GetPixelFormat(TBitmap(Source));
{$ifdef VER9x}
    // Delphi 2 TBitmaps are always DDBs. This means that if a 24 bit
    // bitmap is loaded in 8 bit device mode, TBitmap.PixelFormat will
    // be pf8bit, but TBitmap.Palette will be 0!
    if (TBitmap(Source).Palette = 0) then
      PixelFormat := pfDevice;
{$endif}
    if (PixelFormat > pf8bit) or (PixelFormat = pfDevice) then
    begin
      // Convert image to 8 bits/pixel or less
      FBitmap := ReduceColors(TBitmap(Source), Image.ColorReduction,
        Image.DitherMode, Image.ReductionBits, 0);
      PixelFormat := GetPixelFormat(FBitmap);
    end else
    begin
      // Create new bitmap and copy
      NewBitmap;
      FBitmap.Assign(TBitmap(Source));
    end;

    // Allocate new buffer
    NewImage;

    Image.Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressConverting);
    try
{$ifdef VER9x}
      // This shouldn't happen, but better safe...
      if (FBitmap.Palette = 0) then
        PixelFormat := pf24bit;
{$endif}
      if (not(PixelFormat in [pf1bit, pf4bit, pf8bit, pf24bit])) then
        PixelFormat := pf24bit;
      DIBSource := TDIBReader.Create(FBitmap, PixelFormat);
      try
        // Copy pixels
        case (PixelFormat) of
          pf8bit: Import8Bit(Fdata);
          pf4bit: Import4Bit(Fdata);
          pf1bit: Import1Bit(Fdata);
        else
//        Error(sUnsupportedBitmap);
          Import24Bit(Fdata);
        end;

      finally
        DIBSource.Free;
      end;

{$ifdef VER10_PLUS}
      // Add mask for transparent bitmaps
      if (TBitmap(Source).Transparent) then
        AddMaskOnly(TBitmap(Source).MaskHandle);
{$endif}

    finally
      if ExceptObject = nil then
        i := 100
      else
        i := 0;
      Image.Progress(Self, psEnding, i, Image.PaletteModified, Rect(0,0,0,0), sProgressConverting);
    end;
  end else
  //
  // TGraphic import
  //
  if (Source is TGraphic) then
  begin
    // Zap existing colormap, extensions and bitmap
    Clear;
    if (TGraphic(Source).Empty) then
      exit;

    ABitmap := TBitmap.Create;
    try
      // Import TIcon and TMetafile by drawing them onto a bitmap...
      // ...and then importing the bitmap recursively
      if (Source is TIcon) or (Source is TMetafile) then
      begin
        try
          ImportViaDraw(ABitmap, TGraphic(Source))
        except
          // If import via TCanvas.Draw fails (which it shouldn't), we try the
          // Assign mechanism instead
          ABitmap.Assign(Source);
        end;
      end else
        try
          ABitmap.Assign(Source);
        except
          // If automatic conversion to bitmap fails, we try and draw the
          // graphic on the bitmap instead
          ImportViaDraw(ABitmap, TGraphic(Source));
        end;
      // Convert the bitmap to a GIF frame recursively
      Assign(ABitmap);
    finally
      ABitmap.Free;
    end;

    // Import transparency mask
    if (Source is TIcon) then
      AddIconMask(TIcon(Source));
    if (Source is TMetaFile) then
      AddMetafileMask(TMetaFile(Source));

  end else
  //
  // TPicture import
  //
  if (Source is TPicture) then
  begin
    // Recursively import TGraphic
    Assign(TPicture(Source).Graphic);
  end else
    // Unsupported format - fall back to Source.AssignTo
    inherited Assign(Source);
end;

// Copied from D3 graphics.pas
// Fixed by Brian Lowe of Acro Technology Inc. 30Jan98
function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; MaskDC: HDC; MaskX,
  MaskY: Integer): Boolean;
const
  ROP_DstCopy		= $00AA0029;
var
  MemDC			,
  OrMaskDC		: HDC;
  MemBmp		,
  OrMaskBmp		: HBITMAP;
  Save			,
  OrMaskSave		: THandle;
  crText, crBack	: TColorRef;
  SavePal		: HPALETTE;

begin
  Result := True;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (SrcW = DstW) and (SrcH = DstH) then
  begin
    MemBmp := GDICheck(CreateCompatibleBitmap(SrcDC, 1, 1));
    MemBmp := SelectObject(MaskDC, MemBmp);
    try
      MaskBlt(DstDC, DstX, DstY, DstW, DstH, SrcDC, SrcX, SrcY, MemBmp, MaskX,
        MaskY, MakeRop4(ROP_DstCopy, SrcCopy));
    finally
      MemBmp := SelectObject(MaskDC, MemBmp);
      DeleteObject(MemBmp);
    end;
    Exit;
  end;

  SavePal := 0;
  MemDC := GDICheck(CreateCompatibleDC(DstDC));
  try
    { Color bitmap for combining OR mask with source bitmap }
    MemBmp := GDICheck(CreateCompatibleBitmap(DstDC, SrcW, SrcH));
    try
      Save := SelectObject(MemDC, MemBmp);
      try
        { This bitmap needs the size of the source but DC of the dest }
        OrMaskDC := GDICheck(CreateCompatibleDC(DstDC));
        try
          { Need a monochrome bitmap for OR mask!! }
          OrMaskBmp := GDICheck(CreateBitmap(SrcW, SrcH, 1, 1, nil));
          try
            OrMaskSave := SelectObject(OrMaskDC, OrMaskBmp);
            try

              // OrMask := 1
              // Original: BitBlt(OrMaskDC, SrcX, SrcY, SrcW, SrcH, OrMaskDC, SrcX, SrcY, WHITENESS);
              // Replacement, but not needed: PatBlt(OrMaskDC, SrcX, SrcY, SrcW, SrcH, WHITENESS);
              // OrMask := OrMask XOR Mask
              // Not needed: BitBlt(OrMaskDC, SrcX, SrcY, SrcW, SrcH, MaskDC, SrcX, SrcY, SrcInvert);
              // OrMask := NOT Mask
              BitBlt(OrMaskDC, SrcX, SrcY, SrcW, SrcH, MaskDC, SrcX, SrcY, NotSrcCopy);

              // Retrieve source palette (with dummy select)
              SavePal := SelectPalette(SrcDC, SystemPalette16, False);
              // Restore source palette
              SelectPalette(SrcDC, SavePal, False);
              // Select source palette into memory buffer
              if SavePal <> 0 then
                SavePal := SelectPalette(MemDC, SavePal, True)
              else
                SavePal := SelectPalette(MemDC, SystemPalette16, True);
              RealizePalette(MemDC);

              // Mem := OrMask
              BitBlt(MemDC, SrcX, SrcY, SrcW, SrcH, OrMaskDC, SrcX, SrcY, SrcCopy);
              // Mem := Mem AND Src
{$IFNDEF GIF_TESTMASK} // Define GIF_TESTMASK if you want to know what it does...
              BitBlt(MemDC, SrcX, SrcY, SrcW, SrcH, SrcDC, SrcX, SrcY, SrcAnd);
{$ELSE}
              StretchBlt(DstDC, DstX, DstY, DstW DIV 2, DstH, MemDC, SrcX, SrcY, SrcW, SrcH, SrcCopy);
              StretchBlt(DstDC, DstX+DstW DIV 2, DstY, DstW DIV 2, DstH, SrcDC, SrcX, SrcY, SrcW, SrcH, SrcCopy);
              exit;
{$ENDIF}
            finally
              if (OrMaskSave <> 0) then
                SelectObject(OrMaskDC, OrMaskSave);
            end;
          finally
            DeleteObject(OrMaskBmp);
          end;
        finally
          DeleteDC(OrMaskDC);
        end;

        crText := SetTextColor(DstDC, $00000000);
        crBack := SetBkColor(DstDC, $00FFFFFF);

        { All color rendering is done at 1X (no stretching),
          then final 2 masks are stretched to dest DC }
        // Neat trick!
        // Dst := Dst AND Mask
        StretchBlt(DstDC, DstX, DstY, DstW, DstH, MaskDC, SrcX, SrcY, SrcW, SrcH, SrcAnd);
        // Dst := Dst OR Mem
        StretchBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, SrcX, SrcY, SrcW, SrcH, SrcPaint);

        SetTextColor(DstDC, crText);
        SetTextColor(DstDC, crBack);

      finally
        if (Save <> 0) then
          SelectObject(MemDC, Save);
      end;
    finally
      DeleteObject(MemBmp);
    end;
  finally
    if (SavePal <> 0) then
      SelectPalette(MemDC, SavePal, False);
    DeleteDC(MemDC);
  end;
end;

procedure TGIFSubImage.Draw(ACanvas: TCanvas; const Rect: TRect;
  DoTransparent, DoTile: boolean);
begin
  if (DoTile) then
    StretchDraw(ACanvas, Rect, DoTransparent, DoTile)
  else
    StretchDraw(ACanvas, ScaleRect(Rect), DoTransparent, DoTile);
end;

type
  // Dummy class used to gain access to protected method TCanvas.Changed
  TChangeableleCanvas = class(TCanvas)
  end;

procedure TGIFSubImage.StretchDraw(ACanvas: TCanvas; const Rect: TRect;
  DoTransparent, DoTile: boolean);
var
  MaskDC		: HDC;
  Save			: THandle;
  Tile			: TRect;
{$ifdef DEBUG_DRAWPERFORMANCE}
  ImageCount		,
  TimeStart		,
  TimeStop		: DWORD;
{$endif}

begin
{$ifdef DEBUG_DRAWPERFORMANCE}
  TimeStart := timeGetTime;
  ImageCount := 0;
{$endif}
  if (DoTransparent) and (Transparent) and (HasMask) then
  begin
    // Draw transparent using mask
    Save := 0;
    MaskDC := 0;
    try
      MaskDC := GDICheck(CreateCompatibleDC(0));
      Save := SelectObject(MaskDC, FMask);

      if (DoTile) then
      begin
        Tile.Left := Rect.Left+Left;
        Tile.Right := Tile.Left + Width;
        while (Tile.Left < Rect.Right) do
        begin
          Tile.Top := Rect.Top+Top;
          Tile.Bottom := Tile.Top + Height;
          while (Tile.Top < Rect.Bottom) do
          begin
            TransparentStretchBlt(ACanvas.Handle, Tile.Left, Tile.Top, Width, Height,
              Bitmap.Canvas.Handle, 0, 0, Width, Height, MaskDC, 0, 0);
            Tile.Top := Tile.Top + Image.Height;
            Tile.Bottom := Tile.Bottom + Image.Height;
{$ifdef DEBUG_DRAWPERFORMANCE}
            inc(ImageCount);
{$endif}
          end;
          Tile.Left := Tile.Left + Image.Width;
          Tile.Right := Tile.Right + Image.Width;
        end;
      end else
        TransparentStretchBlt(ACanvas.Handle, Rect.Left, Rect.Top,
          Rect.Right - Rect.Left, Rect.Bottom - Rect.Top,
          Bitmap.Canvas.Handle, 0, 0, Width, Height, MaskDC, 0, 0);

      // Since we are not using any of the TCanvas functions (only handle)
      // we need to fire the TCanvas.Changed method "manually".
      TChangeableCanvas(ACanvas).Changed;

    finally
      if (Save <> 0) then
        SelectObject(MaskDC, Save);
      if (MaskDC <> 0) then
        DeleteDC(MaskDC);
    end;
  end else
  begin
    if (DoTile) then
    begin
      Tile.Left := Rect.Left+Left;
      Tile.Right := Tile.Left + Width;
      while (Tile.Left < Rect.Right) do
      begin
        Tile.Top := Rect.Top+Top;
        Tile.Bottom := Tile.Top + Height;
        while (Tile.Top < Rect.Bottom) do
        begin
          ACanvas.StretchDraw(Tile, Bitmap);
          Tile.Top := Tile.Top + Image.Height;
          Tile.Bottom := Tile.Bottom + Image.Height;
{$ifdef DEBUG_DRAWPERFORMANCE}
          inc(ImageCount);
{$endif}
        end;
        Tile.Left := Tile.Left + Image.Width;
        Tile.Right := Tile.Right + Image.Width;
      end;
    end else
      ACanvas.StretchDraw(Rect, Bitmap);
  end;
{$ifdef DEBUG_DRAWPERFORMANCE}
  if (GetAsyncKeyState(VK_CONTROL) <> 0) then
  begin
    TimeStop := timeGetTime;
    ShowMessage(format('Draw %d images in %d mS, Rate %d images/mS (%d images/S)',
      [ImageCount, TimeStop-TimeStart,
      ImageCount DIV (TimeStop-TimeStart+1),
      MulDiv(ImageCount, 1000, TimeStop-TimeStart+1)]));
  end;
{$endif}
end;

// Given a destination rect (DestRect) calculates the
// area covered by this sub image
function TGIFSubImage.ScaleRect(DestRect: TRect): TRect;
var
  HeightMul		,
  HeightDiv		: integer;
  WidthMul		,
  WidthDiv		: integer;
begin
  HeightDiv := Image.Height;
  HeightMul := DestRect.Bottom-DestRect.Top;
  WidthDiv := Image.Width;
  WidthMul := DestRect.Right-DestRect.Left;

  Result.Left := DestRect.Left + muldiv(Left, WidthMul, WidthDiv);
  Result.Top := DestRect.Top + muldiv(Top, HeightMul, HeightDiv);
  Result.Right := DestRect.Left + muldiv(Left+Width, WidthMul, WidthDiv);
  Result.Bottom := DestRect.Top + muldiv(Top+Height, HeightMul, HeightDiv);
end;

procedure TGIFSubImage.Crop;
var
  TransparentColorIndex	: byte;
  CropLeft		,
  CropTop		,
  CropRight		,
  CropBottom		: integer;
  WasTransparent	: boolean;
  i			: integer;
  NewSize		: integer;
  NewData		: PChar;
  NewWidth		,
  NewHeight		: integer;
  pSource		,
  pDest			: PChar;
begin
  if (Empty) or (not Transparent) then
    exit;
  TransparentColorIndex := GraphicControlExtension.TransparentColorIndex;
  CropLeft := 0;
  CropRight := Width - 1;
  CropTop := 0;
  CropBottom := Height - 1;
  // Find left edge
  WasTransparent := True;
  while (CropLeft <= CropRight) and (WasTransparent) do
  begin
    for i := CropTop to CropBottom do
      if (Pixels[CropLeft, i] <> TransparentColorIndex) then
      begin
        WasTransparent := False;
        break;
      end;
    if (WasTransparent) then
      inc(CropLeft);
  end;
  // Find right edge
  WasTransparent := True;
  while (CropLeft <= CropRight) and (WasTransparent) do
  begin
    for i := CropTop to CropBottom do
      if (pixels[CropRight, i] <> TransparentColorIndex) then
      begin
        WasTransparent := False;
        break;
      end;
    if (WasTransparent) then
      dec(CropRight);
  end;
  if (CropLeft <= CropRight) then
  begin
    // Find top edge
    WasTransparent := True;
    while (CropTop <= CropBottom) and (WasTransparent) do
    begin
      for i := CropLeft to CropRight do
        if (pixels[i, CropTop] <> TransparentColorIndex) then
        begin
          WasTransparent := False;
          break;
        end;
      if (WasTransparent) then
        inc(CropTop);
    end;
    // Find bottom edge
    WasTransparent := True;
    while (CropTop <= CropBottom) and (WasTransparent) do
    begin
      for i := CropLeft to CropRight do
        if (pixels[i, CropBottom] <> TransparentColorIndex) then
        begin
          WasTransparent := False;
          break;
        end;
      if (WasTransparent) then
        dec(CropBottom);
    end;
  end;

  if (CropLeft > CropRight) or (CropTop > CropBottom) then
  begin
    // Cropped to nothing - frame is invisible
    Clear;
  end else
  begin
    // Crop frame - move data
    NewWidth := CropRight - CropLeft + 1;
    Newheight := CropBottom - CropTop + 1;
    NewSize := NewWidth * NewHeight;
    GetMem(NewData, NewSize);
    pSource := PChar(integer(FData) + CropTop * Width + CropLeft);
    pDest := NewData;
    for i := 0 to NewHeight-1 do
    begin
      Move(pSource^, pDest^, NewWidth);
      inc(pSource, Width);
      inc(pDest, NewWidth);
    end;
    FreeImage;
    FData := NewData;
    FDataSize := NewSize;
    inc(FImageDescriptor.Left, CropLeft);
    inc(FImageDescriptor.Top, CropTop);
    FImageDescriptor.Width := NewWidth;
    FImageDescriptor.Height := NewHeight;
    FreeBitmap;
    FreeMask
  end;
end;

procedure TGIFSubImage.Merge(Previous: TGIFSubImage);
var
  SourceIndex		,
  DestIndex		: byte;
  SourceTransparent	: boolean;
  NeedTransparentColorIndex: boolean;
  PreviousRect		,
  ThisRect		,
  MergeRect		: TRect;
  PreviousY		,
  X			,
  Y			: integer;
  pSource		,
  pDest			: PChar;
  pSourceMap		,
  pDestMap		: PColorMap;
  GCE			: TGIFGraphicControlExtension;

  function CanMakeTransparent: boolean;
  begin
    // Is there a local color map...
    if (ColorMap.Count > 0) then
      // ...and is there room in it?
      Result := (ColorMap.Count < 256)
    // Is there a global color map...
    else if (Image.GlobalColorMap.Count > 0) then
      // ...and is there room in it?
      Result := (Image.GlobalColorMap.Count < 256)
    else
      Result := False;
  end;

  function GetTransparentColorIndex: byte;
  var
    i			: integer;
  begin
    if (ColorMap.Count > 0) then
    begin
      // Get the transparent color from the local color map
      Result := ColorMap.Add(TColor(0));
    end else
    begin
      // Are any other frames using the global color map for transparency
      for i := 0 to Image.Images.Count-1 do
        if (Image.Images[i] <> self) and (Image.Images[i].Transparent) and
          (Image.Images[i].ColorMap.Count = 0) then
        begin
          // Use the same transparency color as the other frame
          Result := Image.Images[i].GraphicControlExtension.TransparentColorIndex;
          exit;
        end;
      // Get the transparent color from the global color map
      Result := Image.GlobalColorMap.Add(TColor(0));
    end;
  end;

begin
  // Determine if it is possible to merge this frame
  if (Empty) or (Previous = nil) or (Previous.Empty) or
    ((Previous.GraphicControlExtension <> nil) and
     (Previous.GraphicControlExtension.Disposal in [dmBackground, dmPrevious])) then
    exit;

  PreviousRect := Previous.BoundsRect;
  ThisRect := BoundsRect;

  // Cannot merge unless the frames intersect
  if (not IntersectRect(MergeRect, PreviousRect, ThisRect)) then
    exit;

  // If the frame isn't already transparent, determine
  // if it is possible to make it so
  if (Transparent) then
  begin
    DestIndex := GraphicControlExtension.TransparentColorIndex;
    NeedTransparentColorIndex := False;
  end else
  begin
    if (not CanMakeTransparent) then
      exit;
    DestIndex := 0; // To avoid compiler warning
    NeedTransparentColorIndex := True;
  end;

  SourceTransparent := Previous.Transparent;
  if (SourceTransparent) then
    SourceIndex := Previous.GraphicControlExtension.TransparentColorIndex
  else
    SourceIndex := 0; // To avoid compiler warning

  PreviousY := MergeRect.Top - Previous.Top;

  pSourceMap := Previous.ActiveColorMap.Data;
  pDestMap := ActiveColorMap.Data;

  for Y := MergeRect.Top - Top to MergeRect.Bottom - Top-1 do
  begin
    pSource := PChar(integer(Previous.Scanline[PreviousY]) + MergeRect.Left - Previous.Left);
    pDest := PChar(integer(Scanline[Y]) + MergeRect.Left - Left);

    for X := MergeRect.Left to MergeRect.Right-1 do
    begin
      // Ignore pixels if either this frame's or the previous frame's pixel is transparent
      if (
            not(
              ((not NeedTransparentColorIndex) and (pDest^ = char(DestIndex))) or
              ((SourceTransparent) and (pSource^ = char(SourceIndex)))
            )
          ) and (
            // Replace same colored pixels with transparency
            ((pDestMap = pSourceMap) and (pDest^ = pSource^)) or
            (CompareMem(@(pDestMap^[ord(pDest^)]), @(pSourceMap^[ord(pSource^)]), sizeof(TGIFColor)))
          ) then
      begin
        if (NeedTransparentColorIndex) then
        begin
          NeedTransparentColorIndex := False;
          DestIndex := GetTransparentColorIndex;
        end;
        pDest^ := char(DestIndex);
      end;
      inc(pDest);
      inc(pSource);
    end;
    inc(PreviousY);
  end;

  (*
  ** Create a GCE if the frame wasn't already transparent and any
  ** pixels were made transparent
  *)
  if (not Transparent) and (not NeedTransparentColorIndex) then
  begin
    if (GraphicControlExtension = nil) then
    begin
      GCE := TGIFGraphicControlExtension.Create(self);
      Extensions.Add(GCE);
    end else
      GCE := GraphicControlExtension;
    GCE.Transparent := True;
    GCE.TransparentColorIndex := DestIndex;
  end;

  FreeBitmap;
  FreeMask
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFTrailer
//
////////////////////////////////////////////////////////////////////////////////
procedure TGIFTrailer.SaveToStream(Stream: TStream);
begin
  WriteByte(Stream, bsTrailer);
end;

procedure TGIFTrailer.LoadFromStream(Stream: TStream);
var
  b			: BYTE;
begin
  if (Stream.Read(b, 1) <> 1) then
    exit;
  if (b <> bsTrailer) then
    Warning(gsWarning, sBadTrailer);
end;

////////////////////////////////////////////////////////////////////////////////
//
//		TGIFExtension registration database
//
////////////////////////////////////////////////////////////////////////////////
type
  TExtensionLeadIn = packed record
    Introducer: byte;      { always $21 }
    ExtensionLabel: byte;
  end;

  PExtRec = ^TExtRec;
  TExtRec = record
    ExtClass: TGIFExtensionClass;
    ExtLabel: BYTE;
  end;

  TExtensionList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(eLabel: BYTE; eClass: TGIFExtensionClass);
    function FindExt(eLabel: BYTE): TGIFExtensionClass;
    procedure Remove(eClass: TGIFExtensionClass);
  end;

constructor TExtensionList.Create;
begin
  inherited Create;
  Add(bsPlainTextExtension, TGIFTextExtension);
  Add(bsGraphicControlExtension, TGIFGraphicControlExtension);
  Add(bsCommentExtension, TGIFCommentExtension);
  Add(bsApplicationExtension, TGIFApplicationExtension);
end;

destructor TExtensionList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Dispose(PExtRec(Items[I]));
  inherited Destroy;
end;

procedure TExtensionList.Add(eLabel: BYTE; eClass: TGIFExtensionClass);
var
  NewRec: PExtRec;
begin
  New(NewRec);
  with NewRec^ do
  begin
    ExtLabel := eLabel;
    ExtClass := eClass;
  end;
  inherited Add(NewRec);
end;

function TExtensionList.FindExt(eLabel: BYTE): TGIFExtensionClass;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
    with PExtRec(Items[I])^ do
      if ExtLabel = eLabel then
      begin
        Result := ExtClass;
        Exit;
      end;
  Result := nil;
end;

procedure TExtensionList.Remove(eClass: TGIFExtensionClass);
var
  I: Integer;
  P: PExtRec;
begin
  for I := Count-1 downto 0 do
  begin
    P := PExtRec(Items[I]);
    if P^.ExtClass.InheritsFrom(eClass) then
    begin
      Dispose(P);
      Delete(I);
    end;
  end;
end;

var
  ExtensionList: TExtensionList = nil;

function GetExtensionList: TExtensionList;
begin
  if (ExtensionList = nil) then
    ExtensionList := TExtensionList.Create;
  Result := ExtensionList;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFExtension
//
////////////////////////////////////////////////////////////////////////////////
function TGIFExtension.GetVersion: TGIFVersion;
begin
  Result := gv89a;
end;

class procedure TGIFExtension.RegisterExtension(eLabel: BYTE; eClass: TGIFExtensionClass);
begin
  GetExtensionList.Add(eLabel, eClass);
end;

class function TGIFExtension.FindExtension(Stream: TStream): TGIFExtensionClass;
var
  eLabel		: BYTE;
  SubClass		: TGIFExtensionClass;
  Pos			: LongInt;
begin
  Pos := Stream.Position;
  if (Stream.Read(eLabel, 1) <> 1) then
  begin
    Result := nil;
    exit;
  end;
  Result := GetExtensionList.FindExt(eLabel);
  while (Result <> nil) do
  begin
    SubClass := Result.FindSubExtension(Stream);
    if (SubClass = Result) then
      break;
    Result := SubClass;
  end;
  Stream.Position := Pos;
end;

class function TGIFExtension.FindSubExtension(Stream: TStream): TGIFExtensionClass;
begin
  Result := self;
end;

constructor TGIFExtension.Create(ASubImage: TGIFSubImage);
begin
  inherited Create(ASubImage.Image);
  FSubImage := ASubImage;
end;

destructor TGIFExtension.Destroy;
begin
  if (FSubImage <> nil) then
    FSubImage.Extensions.Remove(self);
  inherited Destroy;
end;

procedure TGIFExtension.SaveToStream(Stream: TStream);
var
  ExtensionLeadIn	: TExtensionLeadIn;
begin
  ExtensionLeadIn.Introducer := bsExtensionIntroducer;
  ExtensionLeadIn.ExtensionLabel := ExtensionType;
  Stream.Write(ExtensionLeadIn, sizeof(ExtensionLeadIn));
end;

function TGIFExtension.DoReadFromStream(Stream: TStream): TGIFExtensionType;
var
  ExtensionLeadIn	: TExtensionLeadIn;
begin
  ReadCheck(Stream, ExtensionLeadIn, sizeof(ExtensionLeadIn));
  if (ExtensionLeadIn.Introducer <> bsExtensionIntroducer) then
    Error(sBadExtensionLabel);
  Result := ExtensionLeadIn.ExtensionLabel;
end;

procedure TGIFExtension.LoadFromStream(Stream: TStream);
begin
  // Seek past lead-in
  // Stream.Seek(sizeof(TExtensionLeadIn), soFromCurrent);
  if (DoReadFromStream(Stream) <> ExtensionType) then
    Error(sBadExtensionInstance);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFGraphicControlExtension
//
////////////////////////////////////////////////////////////////////////////////
const
  { Extension flag bit masks }
  efInputFlag		= $02;		{ 00000010 }
  efDisposal		= $1C;		{ 00011100 }
  efTransparent		= $01;		{ 00000001 }
  efReserved		= $E0;		{ 11100000 }

constructor TGIFGraphicControlExtension.Create(ASubImage: TGIFSubImage);
begin
  inherited Create(ASubImage);

  FGCExtension.BlockSize := 4;
  FGCExtension.PackedFields := $00;
  FGCExtension.DelayTime := 0;
  FGCExtension.TransparentColorIndex := 0;
  FGCExtension.Terminator := 0;
  if (ASubImage.FGCE = nil) then
    ASubImage.FGCE := self;
end;

destructor TGIFGraphicControlExtension.Destroy;
begin
  // Clear transparent flag in sub image
  if (Transparent) then
    SubImage.FTransparent := False;

  if (SubImage.FGCE = self) then
    SubImage.FGCE := nil;

  inherited Destroy;
end;

function TGIFGraphicControlExtension.GetExtensionType: TGIFExtensionType;
begin
  Result := bsGraphicControlExtension;
end;

function TGIFGraphicControlExtension.GetTransparent: boolean;
begin
  Result := (FGCExtension.PackedFields AND efTransparent) <> 0;
end;

procedure TGIFGraphicControlExtension.SetTransparent(Value: boolean);
begin
  // Set transparent flag in sub image
  SubImage.FTransparent := Value;
  if (Value) then
    FGCExtension.PackedFields := FGCExtension.PackedFields OR efTransparent
  else
    FGCExtension.PackedFields := FGCExtension.PackedFields AND NOT(efTransparent);
end;

function TGIFGraphicControlExtension.GetTransparentColor: TColor;
begin
  Result := SubImage.ActiveColorMap[TransparentColorIndex];
end;

procedure TGIFGraphicControlExtension.SetTransparentColor(Color: TColor);
begin
  FGCExtension.TransparentColorIndex := Subimage.ActiveColorMap.AddUnique(Color);
end;

function TGIFGraphicControlExtension.GetTransparentColorIndex: BYTE;
begin
  Result := FGCExtension.TransparentColorIndex;
end;

procedure TGIFGraphicControlExtension.SetTransparentColorIndex(Value: BYTE);
begin
  if ((Value >= SubImage.ActiveColorMap.Count) and (SubImage.ActiveColorMap.Count > 0)) then
  begin
    Warning(gsWarning, sBadColorIndex);
    Value := 0;
  end;
  FGCExtension.TransparentColorIndex := Value;
end;

function TGIFGraphicControlExtension.GetDelay: WORD;
begin
  Result := FGCExtension.DelayTime;
end;
procedure TGIFGraphicControlExtension.SetDelay(Value: WORD);
begin
  FGCExtension.DelayTime := Value;
end;

function TGIFGraphicControlExtension.GetUserInput: boolean;
begin
  Result := (FGCExtension.PackedFields AND efInputFlag) <> 0;
end;

procedure TGIFGraphicControlExtension.SetUserInput(Value: boolean);
begin
  if (Value) then
    FGCExtension.PackedFields := FGCExtension.PackedFields OR efInputFlag
  else
    FGCExtension.PackedFields := FGCExtension.PackedFields AND NOT(efInputFlag);
end;

function TGIFGraphicControlExtension.GetDisposal: TDisposalMethod;
begin
  Result := TDisposalMethod((FGCExtension.PackedFields AND efDisposal) SHR 2);
end;

procedure TGIFGraphicControlExtension.SetDisposal(Value: TDisposalMethod);
begin
  FGCExtension.PackedFields := FGCExtension.PackedFields AND NOT(efDisposal)
    OR ((ord(Value) SHL 2) AND efDisposal);
end;

procedure TGIFGraphicControlExtension.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(FGCExtension, sizeof(FGCExtension));
end;

procedure TGIFGraphicControlExtension.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  if (Stream.Read(FGCExtension, sizeof(FGCExtension)) <> sizeof(FGCExtension)) then
  begin
    Warning(gsWarning, sOutOfData);
    exit;
  end;
  // Set transparent flag in sub image
  if (Transparent) then
    SubImage.FTransparent := True;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFTextExtension
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFTextExtension.Create(ASubImage: TGIFSubImage);
begin
  inherited Create(ASubImage);
  FText := TStringList.Create;
  FPlainTextExtension.BlockSize := 12;
  FPlainTextExtension.Left := 0;
  FPlainTextExtension.Top := 0;
  FPlainTextExtension.Width := 0;
  FPlainTextExtension.Height := 0;
  FPlainTextExtension.CellWidth := 0;
  FPlainTextExtension.CellHeight := 0;
  FPlainTextExtension.TextFGColorIndex := 0;
  FPlainTextExtension.TextBGColorIndex := 0;
end;

destructor TGIFTextExtension.Destroy;
begin
  FText.Free;
  inherited Destroy;
end;

function TGIFTextExtension.GetExtensionType: TGIFExtensionType;
begin
  Result := bsPlainTextExtension;
end;

function TGIFTextExtension.GetForegroundColor: TColor;
begin
  Result := SubImage.ColorMap[ForegroundColorIndex];
end;

procedure TGIFTextExtension.SetForegroundColor(Color: TColor);
begin
  ForegroundColorIndex := SubImage.ActiveColorMap.AddUnique(Color);
end;

function TGIFTextExtension.GetBackgroundColor: TColor;
begin
  Result := SubImage.ActiveColorMap[BackgroundColorIndex];
end;

procedure TGIFTextExtension.SetBackgroundColor(Color: TColor);
begin
  BackgroundColorIndex := SubImage.ColorMap.AddUnique(Color);
end;

function TGIFTextExtension.GetBounds(Index: integer): WORD;
begin
  case (Index) of
    1: Result := FPlainTextExtension.Left;
    2: Result := FPlainTextExtension.Top;
    3: Result := FPlainTextExtension.Width;
    4: Result := FPlainTextExtension.Height;
  else
    Result := 0; // To avoid compiler warnings
  end;
end;

procedure TGIFTextExtension.SetBounds(Index: integer; Value: WORD);
begin
  case (Index) of
    1: FPlainTextExtension.Left := Value;
    2: FPlainTextExtension.Top := Value;
    3: FPlainTextExtension.Width := Value;
    4: FPlainTextExtension.Height := Value;
  end;
end;

function TGIFTextExtension.GetCharWidthHeight(Index: integer): BYTE;
begin
  case (Index) of
    1: Result := FPlainTextExtension.CellWidth;
    2: Result := FPlainTextExtension.CellHeight;
  else
    Result := 0; // To avoid compiler warnings
  end;
end;

procedure TGIFTextExtension.SetCharWidthHeight(Index: integer; Value: BYTE);
begin
  case (Index) of
    1: FPlainTextExtension.CellWidth := Value;
    2: FPlainTextExtension.CellHeight := Value;
  end;
end;

function TGIFTextExtension.GetColorIndex(Index: integer): BYTE;
begin
  case (Index) of
    1: Result := FPlainTextExtension.TextFGColorIndex;
    2: Result := FPlainTextExtension.TextBGColorIndex;
  else
    Result := 0; // To avoid compiler warnings
  end;
end;

procedure TGIFTextExtension.SetColorIndex(Index: integer; Value: BYTE);
begin
  case (Index) of
    1: FPlainTextExtension.TextFGColorIndex := Value;
    2: FPlainTextExtension.TextBGColorIndex := Value;
  end;
end;

procedure TGIFTextExtension.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(FPlainTextExtension, sizeof(FPlainTextExtension));
  WriteStrings(Stream, FText);
end;

procedure TGIFTextExtension.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  ReadCheck(Stream, FPlainTextExtension, sizeof(FPlainTextExtension));
  ReadStrings(Stream, FText);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFCommentExtension
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFCommentExtension.Create(ASubImage: TGIFSubImage);
begin
  inherited Create(ASubImage);
  FText := TStringList.Create;
end;

destructor TGIFCommentExtension.Destroy;
begin
  FText.Free;
  inherited Destroy;
end;

function TGIFCommentExtension.GetExtensionType: TGIFExtensionType;
begin
  Result := bsCommentExtension;
end;

procedure TGIFCommentExtension.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  WriteStrings(Stream, FText);
end;

procedure TGIFCommentExtension.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  ReadStrings(Stream, FText);
end;

////////////////////////////////////////////////////////////////////////////////
//
//		TGIFApplicationExtension registration database
//
////////////////////////////////////////////////////////////////////////////////
type
  PAppExtRec = ^TAppExtRec;
  TAppExtRec = record
    AppClass: TGIFAppExtensionClass;
    Ident: TGIFApplicationRec;
  end;

  TAppExtensionList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(eIdent: TGIFApplicationRec; eClass: TGIFAppExtensionClass);
    function FindExt(eIdent: TGIFApplicationRec): TGIFAppExtensionClass;
    procedure Remove(eClass: TGIFAppExtensionClass);
  end;

constructor TAppExtensionList.Create;
const
  NSLoopIdent: array[0..1] of TGIFApplicationRec =
    ((Identifier: 'NETSCAPE'; Authentication: '2.0'),
     (Identifier: 'ANIMEXTS'; Authentication: '1.0'));
begin
  inherited Create;
  Add(NSLoopIdent[0], TGIFAppExtNSLoop);
  Add(NSLoopIdent[1], TGIFAppExtNSLoop);
end;

destructor TAppExtensionList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Dispose(PAppExtRec(Items[I]));
  inherited Destroy;
end;

procedure TAppExtensionList.Add(eIdent: TGIFApplicationRec; eClass: TGIFAppExtensionClass);
var
  NewRec: PAppExtRec;
begin
  New(NewRec);
  NewRec^.Ident := eIdent;
  NewRec^.AppClass := eClass;
  inherited Add(NewRec);
end;

function TAppExtensionList.FindExt(eIdent: TGIFApplicationRec): TGIFAppExtensionClass;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
    with PAppExtRec(Items[I])^ do
      if CompareMem(@Ident, @eIdent, sizeof(TGIFApplicationRec)) then
      begin
        Result := AppClass;
        Exit;
      end;
  Result := nil;
end;

procedure TAppExtensionList.Remove(eClass: TGIFAppExtensionClass);
var
  I: Integer;
  P: PAppExtRec;
begin
  for I := Count-1 downto 0 do
  begin
    P := PAppExtRec(Items[I]);
    if P^.AppClass.InheritsFrom(eClass) then
    begin
      Dispose(P);
      Delete(I);
    end;
  end;
end;

var
  AppExtensionList: TAppExtensionList = nil;

function GetAppExtensionList: TAppExtensionList;
begin
  if (AppExtensionList = nil) then
    AppExtensionList := TAppExtensionList.Create;
  Result := AppExtensionList;
end;

class procedure TGIFApplicationExtension.RegisterExtension(eIdent: TGIFApplicationRec;
  eClass: TGIFAppExtensionClass);
begin
  GetAppExtensionList.Add(eIdent, eClass);
end;

class function TGIFApplicationExtension.FindSubExtension(Stream: TStream): TGIFExtensionClass;
var
  eIdent		: TGIFApplicationRec;
  OldPos		: longInt;
  Size			: BYTE;
begin
  OldPos := Stream.Position;
  Result := nil;
  if (Stream.Read(Size, 1) <> 1) then
    exit;

  // Some old Adobe export filters mistakenly uses a value of 10
  if (Size = 10) then
  begin
    { TODO -oanme -cImprovement : replace with seek or read and check contents = 'Adobe' }
    if (Stream.Read(eIdent, 10) <> 10) then
      exit;
    Result := TGIFUnknownAppExtension;
    exit;
  end else
  if (Size <> sizeof(TGIFApplicationRec)) or
    (Stream.Read(eIdent, sizeof(eIdent)) <> sizeof(eIdent)) then
  begin
    Stream.Position := OldPos;
    Result := inherited FindSubExtension(Stream);
  end else
  begin
    Result := GetAppExtensionList.FindExt(eIdent);
    if (Result = nil) then
      Result := TGIFUnknownAppExtension;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFApplicationExtension
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFApplicationExtension.Create(ASubImage: TGIFSubImage);
begin
  inherited Create(ASubImage);
  FillChar(FIdent, sizeof(FIdent), 0);
end;

destructor TGIFApplicationExtension.Destroy;
begin
  inherited Destroy;
end;

function TGIFApplicationExtension.GetExtensionType: TGIFExtensionType;
begin
  Result := bsApplicationExtension;
end;

function TGIFApplicationExtension.GetAuthentication: string;
begin
  Result := FIdent.Authentication;
end;

procedure TGIFApplicationExtension.SetAuthentication(const Value: string);
begin
  if (Length(Value) < sizeof(TGIFAuthenticationCode)) then
    FillChar(FIdent.Authentication, sizeof(TGIFAuthenticationCode), 32);
  StrLCopy(@(FIdent.Authentication[0]), PChar(Value), sizeof(TGIFAuthenticationCode));
end;

function TGIFApplicationExtension.GetIdentifier: string;
begin
  Result := FIdent.Identifier;
end;

procedure TGIFApplicationExtension.SetIdentifier(const Value: string);
begin
  if (Length(Value) < sizeof(TGIFIdentifierCode)) then
    FillChar(FIdent.Identifier, sizeof(TGIFIdentifierCode), 32);
  StrLCopy(@(FIdent.Identifier[0]), PChar(Value), sizeof(TGIFIdentifierCode));
end;

procedure TGIFApplicationExtension.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  WriteByte(Stream, sizeof(FIdent)); // Block size
  Stream.Write(FIdent, sizeof(FIdent));
  SaveData(Stream);
end;

procedure TGIFApplicationExtension.LoadFromStream(Stream: TStream);
var
  i			: integer;
begin
  inherited LoadFromStream(Stream);
  i := ReadByte(Stream);
  // Some old Adobe export filters mistakenly uses a value of 10
  if (i = 10) then
    FillChar(FIdent, sizeOf(FIdent), 0)
  else
    if (i < 11) then
      Error(sBadBlockSize);

  ReadCheck(Stream, FIdent, sizeof(FIdent));

  Dec(i, sizeof(FIdent));
  // Ignore extra data
  Stream.Seek(i, soFromCurrent);

  // ***FIXME***
  // If self class is TGIFApplicationExtension, this will cause an "abstract
  // error".
  // TGIFApplicationExtension.LoadData should read and ignore rest of block.
  LoadData(Stream);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFUnknownAppExtension
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFBlock.Create(ASize: integer);
begin
  inherited Create;
  FSize := ASize;
  GetMem(FData, FSize);
  FillChar(FData^, FSize, 0);
end;

destructor TGIFBlock.Destroy;
begin
  FreeMem(FData);
  inherited Destroy;
end;

procedure TGIFBlock.SaveToStream(Stream: TStream);
begin
  Stream.Write(FSize, 1);
  Stream.Write(FData^, FSize);
end;

procedure TGIFBlock.LoadFromStream(Stream: TStream);
begin
  ReadCheck(Stream, FData^, FSize);
end;

constructor TGIFUnknownAppExtension.Create(ASubImage: TGIFSubImage);
begin
  inherited Create(ASubImage);
  FBlocks := TList.Create;
end;

destructor TGIFUnknownAppExtension.Destroy;
var
  i			: integer;
begin
  for i := 0 to FBlocks.Count-1 do
    TGIFBlock(FBlocks[i]).Free;
  FBlocks.Free;
  inherited Destroy;
end;


procedure TGIFUnknownAppExtension.SaveData(Stream: TStream);
var
  i			: integer;
begin
  for i := 0 to FBlocks.Count-1 do
    TGIFBlock(FBlocks[i]).SaveToStream(Stream);
  // Terminating zero
  WriteByte(Stream, 0);
end;

procedure TGIFUnknownAppExtension.LoadData(Stream: TStream);
var
  b			: BYTE;
  Block			: TGIFBlock;
  i			: integer;
begin
  // Zap old blocks
  for i := 0 to FBlocks.Count-1 do
    TGIFBlock(FBlocks[i]).Free;
  FBlocks.Clear;

  // Read blocks
  if (Stream.Read(b, 1) <> 1) then
    exit;
  while (b <> 0) do
  begin
    Block := TGIFBlock.Create(b);
    try
      Block.LoadFromStream(Stream);
    except
      Block.Free;
      raise;
    end;
    FBlocks.Add(Block);
    if (Stream.Read(b, 1) <> 1) then
      exit;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFAppExtNSLoop
//
////////////////////////////////////////////////////////////////////////////////
const
  // Netscape sub block types
  nbLoopExtension	= 1;
  nbBufferExtension	= 2;

constructor TGIFAppExtNSLoop.Create(ASubImage: TGIFSubImage);
const
  NSLoopIdent: TGIFApplicationRec = (Identifier: 'NETSCAPE'; Authentication: '2.0');
begin
  inherited Create(ASubImage);
  FIdent := NSLoopIdent;
end;

procedure TGIFAppExtNSLoop.SaveData(Stream: TStream);
begin
  // Write loop count
  WriteByte(Stream, 1 + sizeof(FLoops)); // Size of block
  WriteByte(Stream, nbLoopExtension); // Identify sub block as looping extension data
  Stream.Write(FLoops, sizeof(FLoops)); // Loop count

  // Write buffer size if specified
  if (FBufferSize > 0) then
  begin
    WriteByte(Stream, 1 + sizeof(FBufferSize)); // Size of block
    WriteByte(Stream, nbBufferExtension); // Identify sub block as buffer size data
    Stream.Write(FBufferSize, sizeof(FBufferSize)); // Buffer size
  end;

  WriteByte(Stream, 0); // Terminating zero
end;

procedure TGIFAppExtNSLoop.LoadData(Stream: TStream);
var
  BlockSize		: integer;
  BlockType		: integer;
begin
  // Read size of first block or terminating zero
  BlockSize := ReadByte(Stream);
  while (BlockSize <> 0) do
  begin
    BlockType := ReadByte(Stream);
    dec(BlockSize);

    case (BlockType AND $07) of
      nbLoopExtension:
        begin
          if (BlockSize < sizeof(FLoops)) then
            Error(sInvalidData);
          // Read loop count
          ReadCheck(Stream, FLoops, sizeof(FLoops));
          dec(BlockSize, sizeof(FLoops));
        end;
      nbBufferExtension:
        begin
          if (BlockSize < sizeof(FBufferSize)) then
            Error(sInvalidData);
          // Read buffer size
          ReadCheck(Stream, FBufferSize, sizeof(FBufferSize));
          dec(BlockSize, sizeof(FBufferSize));
        end;
    end;

    // Skip/ignore unread data
    if (BlockSize > 0) then
      Stream.Seek(BlockSize, soFromCurrent);

    // Read size of next block or terminating zero
    BlockSize := ReadByte(Stream);
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//			TGIFImageList
//
////////////////////////////////////////////////////////////////////////////////
function TGIFImageList.GetImage(Index: Integer): TGIFSubImage;
begin
  Result := TGIFSubImage(Items[Index]);
end;

procedure TGIFImageList.SetImage(Index: Integer; SubImage: TGIFSubImage);
begin
  Items[Index] := SubImage;
end;

procedure TGIFImageList.LoadFromStream(Stream: TStream; Parent: TObject);
var
  b			: BYTE;
  SubImage		: TGIFSubImage;
begin
  // Peek ahead to determine block type
  repeat
    if (Stream.Read(b, 1) <> 1) then
      exit;
  until (b <> 0); // Ignore 0 padding (non-compliant)

  while (b <> bsTrailer) do
  begin
    Stream.Seek(-1, soFromCurrent);
    if (b in [bsExtensionIntroducer, bsImageDescriptor]) then
    begin
      SubImage := TGIFSubImage.Create(Parent as TGIFImage);
      try
        SubImage.LoadFromStream(Stream);
        Add(SubImage);
        Image.Progress(Self, psRunning, MulDiv(Stream.Position, 100, Stream.Size),
          GIFImageRenderOnLoad, Rect(0,0,0,0), sProgressLoading);
      except
        SubImage.Free;
        raise;
      end;
    end else
    begin
      Warning(gsWarning, sBadBlock);
      break;
    end;
    repeat
      if (Stream.Read(b, 1) <> 1) then
        exit;
    until (b <> 0); // Ignore 0 padding (non-compliant)
  end;
  Stream.Seek(-1, soFromCurrent);
end;

procedure TGIFImageList.SaveToStream(Stream: TStream);
var
  i			: integer;
begin
  for i := 0 to Count-1 do
  begin
    TGIFItem(Items[i]).SaveToStream(Stream);
    Image.Progress(Self, psRunning, MulDiv((i+1), 100, Count), False, Rect(0,0,0,0), sProgressSaving);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFPainter
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFPainter.CreateRef(Painter: PGIFPainter; AImage: TGIFImage;
  ACanvas: TCanvas; ARect: TRect; Options: TGIFDrawOptions);
begin
  Create(AImage, ACanvas, ARect, Options);
  PainterRef := Painter;
  if (PainterRef <> nil) then
    PainterRef^ := self;
end;

constructor TGIFPainter.Create(AImage: TGIFImage; ACanvas: TCanvas; ARect: TRect;
  Options: TGIFDrawOptions);
var
  i			: integer;
  BackgroundColor	: TColor;
  Disposals		: set of TDisposalMethod;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Onterminate := DoOnTerminate;
  FImage := AImage;
  FCanvas := ACanvas;
  FRect := ARect;
  FActiveImage := -1;
  FDrawOptions := Options;
  FStarted := False;
  BackupBuffer := nil;
  FrameBuffer := nil;
  Background := nil;
  FEventHandle := 0;
  // This should be a parameter, but I think I've got enough of them already...
  FAnimationSpeed := FImage.AnimationSpeed;

  // An event handle is used for animation delays
  if (FDrawOptions >= [goAnimate, goAsync]) and (FImage.Images.Count > 1) and
    (FAnimationSpeed >= 0) then
    FEventHandle := CreateEvent(nil, False, False, nil);

  // Preprocessing of extensions to determine if we need frame buffers
  Disposals := [];
  if (FImage.DrawBackgroundColor = clNone) then
  begin
    if (FImage.GlobalColorMap.Count > 0) then
      BackgroundColor := FImage.BackgroundColor
    else
      BackgroundColor := ColorToRGB(clWindow);
  end else
    BackgroundColor := ColorToRGB(FImage.DrawBackgroundColor);

  // Need background buffer to clear on loop
  if (goClearOnLoop in FDrawOptions) then
    Include(Disposals, dmBackground);

  for i := 0 to FImage.Images.Count-1 do
    if (FImage.Images[i].GraphicControlExtension <> nil) then
      with (FImage.Images[i].GraphicControlExtension) do
        Include(Disposals, Disposal);

  // Need background buffer to draw transparent on background
  if (dmBackground in Disposals) and (goTransparent in FDrawOptions) then
  begin
    Background := TBitmap.Create;
    Background.Height := FRect.Bottom-FRect.Top;
    Background.Width := FRect.Right-FRect.Left;
    // Copy background immediately
    Background.Canvas.CopyMode := cmSrcCopy;
    Background.Canvas.CopyRect(Background.Canvas.ClipRect, FCanvas, FRect);
  end;
  // Need frame- and backup buffer to restore to previous and background
  if ((Disposals * [dmPrevious, dmBackground]) <> []) then
  begin
    BackupBuffer := TBitmap.Create;
    BackupBuffer.Height := FRect.Bottom-FRect.Top;
    BackupBuffer.Width := FRect.Right-FRect.Left;
    BackupBuffer.Canvas.CopyMode := cmSrcCopy;
    BackupBuffer.Canvas.Brush.Color := BackgroundColor;
    BackupBuffer.Canvas.Brush.Style := bsSolid;
{$IFDEF DEBUG}
    BackupBuffer.Canvas.Brush.Color := clBlack;
    BackupBuffer.Canvas.Brush.Style := bsDiagCross;
{$ENDIF}
    // Step 1: Copy destination to backup buffer
    //         Always executed before first frame and only once.
    BackupBuffer.Canvas.CopyRect(BackupBuffer.Canvas.ClipRect, FCanvas, FRect);
    FrameBuffer := TBitmap.Create;
    FrameBuffer.Height := FRect.Bottom-FRect.Top;
    FrameBuffer.Width := FRect.Right-FRect.Left;
    FrameBuffer.Canvas.CopyMode := cmSrcCopy;
    FrameBuffer.Canvas.Brush.Color := BackgroundColor;
    FrameBuffer.Canvas.Brush.Style := bsSolid;
{$IFDEF DEBUG}
    FrameBuffer.Canvas.Brush.Color := clBlack;
    FrameBuffer.Canvas.Brush.Style := bsDiagCross;
{$ENDIF}
  end;
end;

destructor TGIFPainter.Destroy;
begin
  // OnTerminate isn't called if we are running in main thread, so we must call
  // it manually
  if not(goAsync in DrawOptions) then
    DoOnTerminate(self);
  // Reraise any exptions that were eaten in the Execute method
  if (ExceptObject <> nil) then
    raise ExceptObject at ExceptAddress;
  inherited Destroy;
end;

procedure TGIFPainter.SetAnimationSpeed(Value: integer);
begin
  if (Value < 0) then
    Value := 0
  else if (Value > 1000) then
    Value := 1000;
  if (Value <> FAnimationSpeed) then
  begin
    FAnimationSpeed := Value;
    // Signal WaitForSingleObject delay to abort
    if (FEventHandle <> 0) then
      SetEvent(FEventHandle)
    else
      DoRestart := True;
  end;
end;

procedure TGIFPainter.SetActiveImage(const Value: integer);
begin
  if (Value >= 0) and (Value < FImage.Images.Count) then
    FActiveImage := Value;
end;

// Conditional Synchronize
procedure TGIFPainter.DoSynchronize(Method: TThreadMethod);
begin
  if (Terminated) then
    exit;
  if (goAsync in FDrawOptions) then
    // Execute Synchronized if requested...
    Synchronize(Method)
  else
    // ...Otherwise just execute in current thread (probably main thread)
    Method;
end;

// Delete frame buffers - Executed in main thread
procedure TGIFPainter.DoOnTerminate(Sender: TObject);
begin
  // It shouldn't really be nescessary to protect PainterRef in this manner
  // since we are running in the main thread at this point, but I'm a little
  // paranoid about the way PainterRef is being used...
  if Image <> nil then  // 2001.02.23
  begin  // 2001.02.23
  with Image.Painters.LockList do
    try
      // Zap pointer to self and remove from painter list
      if (PainterRef <> nil) and (PainterRef^ = self) then
        PainterRef^ := nil;

    finally
      Image.Painters.UnLockList;
    end;

  Image.Painters.Remove(self);
  FImage := nil;
  end;  // 2001.02.23


  // Free buffers
  if (BackupBuffer <> nil) then
    BackupBuffer.Free;
  if (FrameBuffer <> nil) then
    FrameBuffer.Free;
  if (Background <> nil) then
    Background.Free;

  // Delete event handle
  if (FEventHandle <> 0) then
    CloseHandle(FEventHandle);
end;

// Event "dispatcher" - Executed in main thread
procedure TGIFPainter.DoEvent;
begin
  if (Assigned(FEvent)) then
    FEvent(self);
end;

// Non-buffered paint - Executed in main thread
procedure TGIFPainter.DoPaint;
begin
  FImage.Images[ActiveImage].Draw(FCanvas, FRect, (goTransparent in FDrawOptions),
    (goTile in FDrawOptions));
  FStarted := True;
end;

// Buffered paint - Executed in main thread
procedure TGIFPainter.DoPaintFrame;
var
  DrawDestination	: TCanvas;
  DrawRect		: TRect;
  DoStep2		,
  DoStep3		,
  DoStep5		,
  DoStep6		: boolean;
  SavePal		,
  SourcePal		: HPALETTE;

  procedure ClearBackup;
  var
    r			,
    Tile		: TRect;
    FrameTop		,
    FrameHeight		: integer;
    ImageWidth		,
    ImageHeight		: integer;
  begin

    if (goTransparent in FDrawOptions) then
    begin
      // If the frame is transparent, we must remove it by copying the
      // background buffer over it
      if (goTile in FDrawOptions) then
      begin
        FrameTop := FImage.Images[ActiveImage].Top;
        FrameHeight := FImage.Images[ActiveImage].Height;
        ImageWidth := FImage.Width;
        ImageHeight := FImage.Height;

        Tile.Left := FRect.Left + FImage.Images[ActiveImage].Left;
        Tile.Right := Tile.Left + FImage.Images[ActiveImage].Width;
        while (Tile.Left < FRect.Right) do
        begin
          Tile.Top := FRect.Top + FrameTop;
          Tile.Bottom := Tile.Top + FrameHeight;
          while (Tile.Top < FRect.Bottom) do
          begin
            BackupBuffer.Canvas.CopyRect(Tile, Background.Canvas, Tile);
            Tile.Top := Tile.Top + ImageHeight;
            Tile.Bottom := Tile.Bottom + ImageHeight;
          end;
          Tile.Left := Tile.Left + ImageWidth;
          Tile.Right := Tile.Right + ImageWidth;
        end;
      end else
      begin
        r := FImage.Images[ActiveImage].ScaleRect(BackupBuffer.Canvas.ClipRect);
        BackupBuffer.Canvas.CopyRect(r, Background.Canvas, r)
      end;
    end else
    begin
      // If the frame isn't transparent, we just clear the area covered by
      // it to the background color.
      // Tile the background unless the frame covers all of the image
      if (goTile in FDrawOptions) and
        ((FImage.Width <> FImage.Images[ActiveImage].Width) and
         (FImage.height <> FImage.Images[ActiveImage].Height)) then
      begin
        FrameTop := FImage.Images[ActiveImage].Top;
        FrameHeight := FImage.Images[ActiveImage].Height;
        ImageWidth := FImage.Width;
        ImageHeight := FImage.Height;
        // ***FIXME*** I don't think this does any difference
        BackupBuffer.Canvas.Brush.Color := FImage.DrawBackgroundColor;

        Tile.Left := FRect.Left + FImage.Images[ActiveImage].Left;
        Tile.Right := Tile.Left + FImage.Images[ActiveImage].Width;
        while (Tile.Left < FRect.Right) do
        begin
          Tile.Top := FRect.Top + FrameTop;
          Tile.Bottom := Tile.Top + FrameHeight;
          while (Tile.Top < FRect.Bottom) do
          begin
            BackupBuffer.Canvas.FillRect(Tile);

            Tile.Top := Tile.Top + ImageHeight;
            Tile.Bottom := Tile.Bottom + ImageHeight;
          end;
          Tile.Left := Tile.Left + ImageWidth;
          Tile.Right := Tile.Right + ImageWidth;
        end;
      end else
        BackupBuffer.Canvas.FillRect(FImage.Images[ActiveImage].ScaleRect(FRect));
    end;
  end;

begin
  if (goValidateCanvas in FDrawOptions) then
    if (GetObjectType(ValidateDC) <> OBJ_DC) then
    begin
      Terminate;
      exit;
    end;

  DrawDestination := nil;
  DoStep2 := (goClearOnLoop in FDrawOptions) and (FActiveImage = 0);
  DoStep3 := False;
  DoStep5 := False;
  DoStep6 := False;
{
Disposal mode algorithm:

Step 1: Copy destination to backup buffer
        Always executed before first frame and only once.
        Done in constructor.
Step 2: Clear previous frame (implementation is same as step 6)
        Done implicitly by implementation.
        Only done explicitly on first frame if goClearOnLoop option is set.
Step 3: Copy backup buffer to frame buffer
Step 4: Draw frame
Step 5: Copy buffer to destination
Step 6: Clear frame from backup buffer
+------------+------------------+---------------------+------------------------+
|New  \  Old |  dmNone          |  dmBackground       |  dmPrevious            |
+------------+------------------+---------------------+------------------------+
|dmNone      |                  |                     |                        |
|            |4. Paint on backup|4. Paint on backup   |4. Paint on backup      |
|            |5. Restore        |5. Restore           |5. Restore              |
+------------+------------------+---------------------+------------------------+
|dmBackground|                  |                     |                        |
|            |4. Paint on backup|4. Paint on backup   |4. Paint on backup      |
|            |5. Restore        |5. Restore           |5. Restore              |
|            |6. Clear backup   |6. Clear backup      |6. Clear backup         |
+------------+------------------+---------------------+------------------------+
|dmPrevious  |                  |                     |                        |
|            |                  |3. Copy backup to buf|3. Copy backup to buf   |
|            |4. Paint on dest  |4. Paint on buf      |4. Paint on buf         |
|            |                  |5. Copy buf to dest  |5. Copy buf to dest     |
+------------+------------------+---------------------+------------------------+
}
  case (Disposal) of
    dmNone, dmNoDisposal:
      begin
        DrawDestination := BackupBuffer.Canvas;
        DrawRect := BackupBuffer.Canvas.ClipRect;
        DoStep5 := True;
      end;
    dmBackground:
      begin
        DrawDestination := BackupBuffer.Canvas;
        DrawRect := BackupBuffer.Canvas.ClipRect;
        DoStep5 := True;
        DoStep6 := True;
      end;
    dmPrevious:
      case (OldDisposal) of
        dmNone, dmNoDisposal:
        begin
          DrawDestination := FCanvas;
          DrawRect := FRect;
        end;
        dmBackground, dmPrevious:
        begin
          DrawDestination := FrameBuffer.Canvas;
          DrawRect := FrameBuffer.Canvas.ClipRect;
          DoStep3 := True;
          DoStep5 := True;
        end;
      end;
  end;

  // Find source palette
  SourcePal := FImage.Images[ActiveImage].Palette;
  if (SourcePal = 0) then
    SourcePal := SystemPalette16; // This should never happen

  SavePal := SelectPalette(DrawDestination.Handle, SourcePal, False);
  RealizePalette(DrawDestination.Handle);

  // Step 2: Clear previous frame
  if (DoStep2) then
   ClearBackup;

  // Step 3: Copy backup buffer to frame buffer
  if (DoStep3) then
    FrameBuffer.Canvas.CopyRect(FrameBuffer.Canvas.ClipRect,
      BackupBuffer.Canvas, BackupBuffer.Canvas.ClipRect);

  // Step 4: Draw frame
  if (DrawDestination <> nil) then
    FImage.Images[ActiveImage].Draw(DrawDestination, DrawRect,
      (goTransparent in FDrawOptions), (goTile in FDrawOptions));

  // Step 5: Copy buffer to destination
  if (DoStep5) then
  begin
    FCanvas.CopyMode := cmSrcCopy;
    FCanvas.CopyRect(FRect, DrawDestination, DrawRect);
  end;

  if (SavePal <> 0) then
    SelectPalette(DrawDestination.Handle, SavePal, False);

  // Step 6: Clear frame from backup buffer
  if (DoStep6) then
   ClearBackup;

  FStarted := True;
end;

// Prefetch bitmap
// Used to force the GIF image to be rendered as a bitmap
{$ifdef SERIALIZE_RENDER}
procedure TGIFPainter.PrefetchBitmap;
begin
  // Touch current bitmap to force bitmap to be rendered
  if not((FImage.Images[ActiveImage].Empty) or (FImage.Images[ActiveImage].HasBitmap)) then
    FImage.Images[ActiveImage].Bitmap;
end;
{$endif}

// Main thread execution loop - This is where it all happens...
procedure TGIFPainter.Execute;
var
  i			: integer;
  LoopCount		,
  LoopPoint		: integer;
  Looping		: boolean;
  Ext			: TGIFExtension;
  Msg			: TMsg;
  Delay			,
  OldDelay		,
  DelayUsed		: longInt;
  DelayStart		,
  NewDelayStart		: DWORD;

  procedure FireEvent(Event: TNotifyEvent);
  begin
    if not(Assigned(Event)) then
      exit;
    FEvent := Event;
    try
      DoSynchronize(DoEvent);
    finally
      FEvent := nil;
    end;
  end;

begin
{
  Disposal:
    dmNone: Same as dmNodisposal
    dmNoDisposal: Do not dispose
    dmBackground: Clear with background color *)
    dmPrevious: Previous image
    *) Note: Background color should either be a BROWSER SPECIFIED Background
       color (DrawBackgroundColor) or the background image if any frames are
       transparent.
}
  try
    try
      if (goValidateCanvas in FDrawOptions) then
        ValidateDC := FCanvas.Handle;
      DoRestart := True;

      // Loop to restart paint
      while (DoRestart) and not(Terminated) do
      begin
        FActiveImage := 0;
        // Fire OnStartPaint event
        // Note: ActiveImage may be altered by the event handler
        FireEvent(FOnStartPaint);

        FStarted := False;
        DoRestart := False;
        LoopCount := 1;
        LoopPoint := FActiveImage;
        Looping := False;
        if (goAsync in DrawOptions) then
          Delay := 0
        else
          Delay := 1; // Dummy to process messages
        OldDisposal := dmNoDisposal;
        // Fetch delay start time
        DelayStart := timeGetTime;
        OldDelay := 0;

        // Loop to loop - duh!
        while ((LoopCount <> 0) or (goLoopContinously in DrawOptions)) and
          not(Terminated or DoRestart) do
        begin
          FActiveImage := LoopPoint;

          // Fire OnLoopPaint event
          // Note: ActiveImage may be altered by the event handler
          if (FStarted) then
            FireEvent(FOnLoop);

          // Loop to animate
          while (ActiveImage < FImage.Images.Count) and not(Terminated or DoRestart) do
          begin
            // Ignore empty images
            if (FImage.Images[ActiveImage].Empty) then
              break;
            // Delay from previous image
            if (Delay > 0) then
            begin
              // Prefetch frame bitmap
{$ifdef SERIALIZE_RENDER}
              DoSynchronize(PrefetchBitmap);
{$else}
              FImage.Images[ActiveImage].Bitmap;
{$endif}

              // Calculate inter frame delay
              NewDelayStart := timeGetTime;
              if (FAnimationSpeed > 0) then
              begin
                // Calculate number of mS used in prefetch and display
                try
                  DelayUsed := integer(NewDelayStart-DelayStart)-OldDelay;
                  // Prevent feedback oscillations caused by over/undercompensation.
                  DelayUsed := DelayUsed DIV 2;
                  // Convert delay value to mS and...
                  // ...Adjust for time already spent converting GIF to bitmap and...
                  // ...Adjust for Animation Speed factor.
                  Delay := MulDiv(Delay * GIFDelayExp - DelayUsed, 100, FAnimationSpeed);
                  OldDelay := Delay;
                except
                  Delay := GIFMaximumDelay * GIFDelayExp;
                  OldDelay := 0;
                end;
              end else
              begin
                if (goAsync in DrawOptions) then
                  Delay := longInt(INFINITE)
                else
                  Delay := GIFMaximumDelay * GIFDelayExp;
              end;
              // Fetch delay start time
              DelayStart := NewDelayStart;

              // Sleep in one chunk if we are running in a thread
              if (goAsync in DrawOptions) then
              begin
                // Use of WaitForSingleObject allows TGIFPainter.Stop to wake us up
                if (Delay > 0) or (FAnimationSpeed = 0) then
                begin
                  if (WaitForSingleObject(FEventHandle, DWORD(Delay)) <> WAIT_TIMEOUT) then
                  begin
                    // Don't use interframe delay feedback adjustment if delay
                    // were prematurely aborted (e.g. because the animation
                    // speed were changed)
                    OldDelay := 0;
                    DelayStart := longInt(timeGetTime);
                  end;
                end;
              end else
              begin
                if (Delay <= 0) then
                  Delay := 1;
                // Fetch start time
                NewDelayStart := timeGetTime;
                // If we are not running in a thread we Sleep in small chunks
                // and give the user a chance to abort
                while (Delay > 0) and not(Terminated or DoRestart) do
                begin
                  if (Delay < 100) then
                    Sleep(Delay)
                  else
                    Sleep(100);
                  // Calculate number of mS delayed in this chunk
                  DelayUsed := integer(timeGetTime - NewDelayStart);
                  dec(Delay, DelayUsed);
                  // Reset start time for chunk
                  NewDelaySTart := timeGetTime;
                  // Application.ProcessMessages wannabe
                  while (not(Terminated or DoRestart)) and
                    (PeekMessage(Msg, 0, 0, 0, PM_REMOVE)) do
                  begin
                    if (Msg.Message <> WM_QUIT) then
                    begin
                      TranslateMessage(Msg);
                      DispatchMessage(Msg);
                    end else
                    begin
                      // Put WM_QUIT back in queue and get out of here fast
                      PostQuitMessage(Msg.WParam);
                      Terminate;
                    end;
                  end;
                end;
              end;
            end else
              Sleep(0); // Yield
            if (Terminated) then
              break;

            // Fire OnPaint event
            // Note: ActiveImage may be altered by the event handler
            FireEvent(FOnPaint);
            if (Terminated) then
              break;

            // Pre-draw processing of extensions
            Disposal := dmNoDisposal;
            for i := 0 to FImage.Images[ActiveImage].Extensions.Count-1 do
            begin
              Ext := FImage.Images[ActiveImage].Extensions[i];
              if (Ext is TGIFAppExtNSLoop) then
              begin
                // Recursive loops not supported (or defined)
                if (Looping) then
                  continue;
                Looping := True;
                LoopCount := TGIFAppExtNSLoop(Ext).Loops;
                if ((LoopCount = 0) or (goLoopContinously in DrawOptions)) and
                  (goAsync in DrawOptions) then
                  LoopCount := -1; // Infinite if running in separate thread
{$IFNDEF STRICT_MOZILLA}
                // Loop from this image and on
                // Note: This is not standard behavior
                LoopPoint := ActiveImage;
{$ENDIF}
              end else
              if (Ext is TGIFGraphicControlExtension) then
                Disposal := TGIFGraphicControlExtension(Ext).Disposal;
            end;

            // Paint the image
            if (BackupBuffer <> nil) then
              DoSynchronize(DoPaintFrame)
            else
              DoSynchronize(DoPaint);
            OldDisposal := Disposal;

            if (Terminated) then
              break;

            Delay := GIFDefaultDelay; // Default delay
            // Post-draw processing of extensions
            if (FImage.Images[ActiveImage].GraphicControlExtension <> nil) then
              if (FImage.Images[ActiveImage].GraphicControlExtension.Delay > 0) then
              begin
                Delay := FImage.Images[ActiveImage].GraphicControlExtension.Delay;

                // Enforce minimum animation delay in compliance with Mozilla
                if (Delay < GIFMinimumDelay) then
                  Delay := GIFMinimumDelay;

                // Do not delay more than 10 seconds if running in main thread
                if (Delay > GIFMaximumDelay) and not(goAsync in DrawOptions) then
                  Delay := GIFMaximumDelay; // Max 10 seconds
              end;
            // Fire OnAfterPaint event
            // Note: ActiveImage may be altered by the event handler
            i := FActiveImage;
            FireEvent(FOnAfterPaint);
            if (Terminated) then
              break;
            // Don't increment frame counter if event handler modified
            // current frame
            if (FActiveImage = i) then
              Inc(FActiveImage);
            // Nothing more to do unless we are animating
            if not(goAnimate in DrawOptions) then
              break;
          end;

          if (LoopCount > 0) then
            Dec(LoopCount);
          if ([goAnimate, goLoop] * DrawOptions <> [goAnimate, goLoop]) then
            break;
        end;
        if (Terminated) then  // 2001.07.23
          break;  // 2001.07.23
      end;
      FActiveImage := -1;
      // Fire OnEndPaint event
      FireEvent(FOnEndPaint);
    finally
      // If we are running in the main thread we will have to zap our self
      if not(goAsync in DrawOptions) then
        Free;
    end;
  except
    on E: Exception do
    begin
      // Eat exception and terminate thread...
      // If we allow the exception to abort the thread at this point, the
      // application will hang since the thread destructor will never be called
      // and the application will wait forever for the thread to die!
      Terminate;
      // Clone exception
      ExceptObject := E.Create(E.Message);
      ExceptAddress := ExceptAddr;
    end;
  end;
end;

procedure TGIFPainter.Start;
begin
  if (goAsync in FDrawOptions) then
    Resume;
end;

procedure TGIFPainter.Stop;
begin
  Terminate;
  if (goAsync in FDrawOptions) then
  begin
    // Signal WaitForSingleObject delay to abort
    if (FEventHandle <> 0) then
      SetEvent(FEventHandle);
    Priority := tpNormal;
    if (Suspended) then
      Resume; // Must be running before we can terminate
  end;
end;

procedure TGIFPainter.Restart;
begin
  DoRestart := True;
  if (Suspended) and (goAsync in FDrawOptions) then
    Resume; // Must be running before we can terminate
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TColorMapOptimizer
//
////////////////////////////////////////////////////////////////////////////////
// Used by TGIFImage to optimize local color maps to a single global color map.
// The following algorithm is used:
// 1) Build a histogram for each image
// 2) Merge histograms
// 3) Sum equal colors and adjust max # of colors
// 4) Map entries > max to entries <= 256
// 5) Build new color map
// 6) Map images to new color map
////////////////////////////////////////////////////////////////////////////////

type

  POptimizeEntry	= ^TOptimizeEntry;
  TColorRec = record
  case byte of
    0: (Value: integer);
    1: (Color: TGIFColor);
    2: (SameAs: POptimizeEntry); // Used if TOptimizeEntry.Count = 0
  end;

  TOptimizeEntry = record
    Count		: integer;	// Usage count
    OldIndex		: integer;	// Color OldIndex
    NewIndex		: integer;	// NewIndex color OldIndex
    Color		: TColorRec;	// Color value
  end;

  TOptimizeEntries	= array[0..255] of TOptimizeEntry;
  POptimizeEntries	= ^TOptimizeEntries;

  THistogram = class(TObject)
  private
    PHistogram		: POptimizeEntries;
    FCount		: integer;
    FColorMap		: TGIFColorMap;
    FList		: TList;
    FImages		: TList;
  public
    constructor Create(AColorMap: TGIFColorMap);
    destructor Destroy; override;
    function ProcessSubImage(Image: TGIFSubImage): boolean;
    function Prune: integer;
    procedure MapImages(UseTransparency: boolean; NewTransparentColorIndex: byte);
    property Count: integer read FCount;
    property ColorMap: TGIFColorMap read FColorMap;
    property List: TList read FList;
  end;

  TColorMapOptimizer = class(TObject)
  private
    FImage		: TGIFImage;
    FHistogramList	: TList;
    FHistogram		: TList;
    FColorMap		: TColorMap;
    FFinalCount		: integer;
    FUseTransparency	: boolean;
    FNewTransparentColorIndex: byte;
  protected
    procedure ProcessImage;
    procedure MergeColors;
    procedure MapColors;
    procedure ReplaceColorMaps;
  public
    constructor Create(AImage: TGIFImage);
    destructor Destroy; override;
    procedure Optimize;
  end;

function CompareColor(Item1, Item2: Pointer): integer;
begin
  Result := POptimizeEntry(Item2)^.Color.Value - POptimizeEntry(Item1)^.Color.Value;
end;

function CompareCount(Item1, Item2: Pointer): integer;
begin
  Result := POptimizeEntry(Item2)^.Count - POptimizeEntry(Item1)^.Count;
end;

constructor THistogram.Create(AColorMap: TGIFColorMap);
var
  i			: integer;
begin
  inherited Create;

  FCount := AColorMap.Count;
  FColorMap := AColorMap;

  FImages := TList.Create;

  // Allocate memory for histogram
  GetMem(PHistogram, FCount * sizeof(TOptimizeEntry));
  FList := TList.Create;

  FList.Capacity := FCount;

  // Move data to histogram and initialize
  for i := 0 to FCount-1 do
    with PHistogram^[i] do
    begin
      FList.Add(@PHistogram^[i]);
      OldIndex := i;
      Count := 0;
      Color.Value := 0;
      Color.Color := AColorMap.Data^[i];
      NewIndex := 256; // Used to signal unmapped
    end;
end;

destructor THistogram.Destroy;
begin
  FImages.Free;
  FList.Free;
  FreeMem(PHistogram);
  inherited Destroy;
end;

//: Build a color histogram
function THistogram.ProcessSubImage(Image: TGIFSubImage): boolean;
var
  Size			: integer;
  Pixel			: PChar;
  IsTransparent		,
  WasTransparent	: boolean;
  OldTransparentColorIndex: byte;
begin
  Result := False;
  if (Image.Empty) then
    exit;

  FImages.Add(Image);

  Pixel := Image.data;
  Size := Image.Width * Image.Height;

  IsTransparent := Image.Transparent;
  if (IsTransparent) then
    OldTransparentColorIndex := Image.GraphicControlExtension.TransparentColorIndex
  else
    OldTransparentColorIndex := 0; // To avoid compiler warning
  WasTransparent := False;

  (*
  ** Sum up usage count for each color
  *)
  while (Size > 0) do
  begin
    // Ignore transparent pixels
    if (not IsTransparent) or (ord(Pixel^) <> OldTransparentColorIndex) then
    begin
      // Check for invalid color index
      if (ord(Pixel^) >= FCount) then
      begin
        Pixel^ := #0; // ***FIXME*** Isn't this an error condition?
        Image.Warning(gsWarning, sInvalidColor);
      end;

      with PHistogram^[ord(Pixel^)] do
      begin
        // Stop if any color reaches the max count
        if (Count = high(integer)) then
          break;
        inc(Count);
      end;
    end else
      WasTransparent := WasTransparent or IsTransparent;
    inc(Pixel);
    dec(Size);
  end;

  (*
  ** Clear frames transparency flag if the frame claimed to
  ** be transparent, but wasn't
  *)
  if (IsTransparent and not WasTransparent) then
  begin
    Image.GraphicControlExtension.TransparentColorIndex := 0;
    Image.GraphicControlExtension.Transparent := False;
  end;

  Result := WasTransparent;
end;

//: Removed unused color entries from the histogram
function THistogram.Prune: integer;
var
  i, j			: integer;
begin
  (*
  **  Sort by usage count
  *)
  FList.Sort(CompareCount);

  (*
  **  Determine number of used colors
  *)
  for i := 0 to FCount-1 do
    // Find first unused color entry
    if (POptimizeEntry(FList[i])^.Count = 0) then
    begin
      // Zap unused colors
      for j := i to FCount-1 do
        POptimizeEntry(FList[j])^.Count := -1; // Use -1 to signal unused entry
      // Remove unused entries
      FCount := i;
      FList.Count := FCount;
      break;
    end;

  Result := FCount;
end;

//: Convert images from old color map to new color map
procedure THistogram.MapImages(UseTransparency: boolean; NewTransparentColorIndex: byte);
var
  i			: integer;
  Size			: integer;
  Pixel			: PChar;
  ReverseMap		: array[byte] of byte;
  IsTransparent		: boolean;
  OldTransparentColorIndex: byte;
begin
  (*
  ** Build NewIndex map
  *)
  for i := 0 to List.Count-1 do
    ReverseMap[POptimizeEntry(List[i])^.OldIndex] := POptimizeEntry(List[i])^.NewIndex;

  (*
  **  Reorder all images using this color map
  *)
  for i := 0 to FImages.Count-1 do
    with TGIFSubImage(FImages[i]) do
    begin
      Pixel := Data;
      Size := Width * Height;

      // Determine frame transparency
      IsTransparent := (Transparent) and (UseTransparency);
      if (IsTransparent) then
      begin
        OldTransparentColorIndex := GraphicControlExtension.TransparentColorIndex;
        // Map transparent color
        GraphicControlExtension.TransparentColorIndex := NewTransparentColorIndex;
      end else
        OldTransparentColorIndex := 0; // To avoid compiler warning

      // Map all pixels to new color map
      while (Size > 0) do
      begin
        // Map transparent pixels to the new transparent color index and...
        if (IsTransparent) and (ord(Pixel^) = OldTransparentColorIndex) then
          Pixel^ := char(NewTransparentColorIndex)
        else
          // ... all other pixels to their new color index
          Pixel^ := char(ReverseMap[ord(Pixel^)]);
        dec(size);
        inc(Pixel);
      end;
    end;
end;

constructor TColorMapOptimizer.Create(AImage: TGIFImage);
begin
  inherited Create;
  FImage := AImage;
  FHistogramList := TList.Create;
  FHistogram := TList.Create;
end;

destructor TColorMapOptimizer.Destroy;
var
  i			: integer;
begin
  FHistogram.Free;

  for i := FHistogramList.Count-1 downto 0 do
    THistogram(FHistogramList[i]).Free;
  FHistogramList.Free;

  inherited Destroy;
end;

procedure TColorMapOptimizer.ProcessImage;
var
  Hist			: THistogram;
  i			: integer;
  ProcessedImage	: boolean;
begin
  FUseTransparency := False;
  (*
  ** First process images using global color map
  *)
  if (FImage.GlobalColorMap.Count > 0) then
  begin
    Hist := THistogram.Create(FImage.GlobalColorMap);
    ProcessedImage := False;
    // Process all images that are using the global color map
    for i := 0 to FImage.Images.Count-1 do
      if (FImage.Images[i].ColorMap.Count = 0) and (not FImage.Images[i].Empty) then
      begin
        ProcessedImage := True;
      // Note: Do not change order of statements. Shortcircuit evaluation not desired!
        FUseTransparency := Hist.ProcessSubImage(FImage.Images[i]) or FUseTransparency;
      end;
    // Keep the histogram if any images used the global color map...
    if (ProcessedImage) then
      FHistogramList.Add(Hist)
    else // ... otherwise delete it
      Hist.Free;
  end;

  (*
  ** Next process images that have a local color map
  *)
  for i := 0 to FImage.Images.Count-1 do
    if (FImage.Images[i].ColorMap.Count > 0) and (not FImage.Images[i].Empty) then
    begin
      Hist := THistogram.Create(FImage.Images[i].ColorMap);
      FHistogramList.Add(Hist);
      // Note: Do not change order of statements. Shortcircuit evaluation not desired!
      FUseTransparency := Hist.ProcessSubImage(FImage.Images[i]) or FUseTransparency;
    end;
end;

procedure TColorMapOptimizer.MergeColors;
var
  Entry, SameEntry	: POptimizeEntry;
  i			: integer;
begin
  (*
  **  Sort by color value
  *)
  FHistogram.Sort(CompareColor);

  (*
  **  Merge same colors
  *)
  SameEntry := POptimizeEntry(FHistogram[0]);
  for i := 1 to FHistogram.Count-1 do
  begin
    Entry := POptimizeEntry(FHistogram[i]);
    ASSERT(Entry^.Count > 0, 'Unused entry exported from THistogram');
    if (Entry^.Color.Value = SameEntry^.Color.Value) then
    begin
      // Transfer usage count to first entry
      inc(SameEntry^.Count, Entry^.Count);
      Entry^.Count := 0; // Use 0 to signal merged entry
      Entry^.Color.SameAs := SameEntry; // Point to master
    end else
      SameEntry := Entry;
  end;
end;

procedure TColorMapOptimizer.MapColors;
var
  i, j			: integer;
  Delta, BestDelta	: integer;
  BestIndex		: integer;
  MaxColors		: integer;
begin
  (*
  **  Sort by usage count
  *)
  FHistogram.Sort(CompareCount);

  (*
  ** Handle transparency
  *)
  if (FUseTransparency) then
    MaxColors := 255
  else
    MaxColors := 256;

  (*
  **  Determine number of colors used (max 256)
  *)
  FFinalCount := FHistogram.Count;
  for i := 0 to FFinalCount-1 do
    if (i >= MaxColors) or (POptimizeEntry(FHistogram[i])^.Count = 0) then
    begin
      FFinalCount := i;
      break;
    end;

  (*
  **  Build color map and reverse map for final entries
  *)
  for i := 0 to FFinalCount-1 do
  begin
    POptimizeEntry(FHistogram[i])^.NewIndex := i;
    FColorMap[i] := POptimizeEntry(FHistogram[i])^.Color.Color;
  end;

  (*
  **  Map colors > 256 to colors <= 256 and build NewIndex color map
  *)
  for i := FFinalCount to FHistogram.Count-1 do
    with POptimizeEntry(FHistogram[i])^ do
    begin
      // Entries with a usage count of -1 is unused
      ASSERT(Count <> -1, 'Internal error: Unused entry exported');
      // Entries with a usage count of 0 has been merged with another entry
      if (Count = 0) then
      begin
        // Use mapping of master entry
        ASSERT(Color.SameAs.NewIndex < 256, 'Internal error: Mapping to unmapped color');
        NewIndex := Color.SameAs.NewIndex;
      end else
      begin
        // Search for entry with nearest color value
        BestIndex := 0;
        BestDelta := 255*3;
        for j := 0 to FFinalCount-1 do
        begin
          Delta := ABS((POptimizeEntry(FHistogram[j])^.Color.Color.Red - Color.Color.Red) +
            (POptimizeEntry(FHistogram[j])^.Color.Color.Green - Color.Color.Green) +
            (POptimizeEntry(FHistogram[j])^.Color.Color.Blue - Color.Color.Blue));
          if (Delta < BestDelta) then
          begin
            BestDelta := Delta;
            BestIndex := j;
          end;
        end;
        NewIndex := POptimizeEntry(FHistogram[BestIndex])^.NewIndex;;
      end;
    end;

  (*
  ** Add transparency color to new color map
  *)
  if (FUseTransparency) then
  begin
    FNewTransparentColorIndex := FFinalCount;
    FColorMap[FFinalCount].Red := 0;
    FColorMap[FFinalCount].Green := 0;
    FColorMap[FFinalCount].Blue := 0;
    inc(FFinalCount);
  end;
end;

procedure TColorMapOptimizer.ReplaceColorMaps;
var
  i			: integer;
begin
  // Zap all local color maps
  for i := 0 to FImage.Images.Count-1 do
    if (FImage.Images[i].ColorMap <> nil) then
      FImage.Images[i].ColorMap.Clear;
  // Store optimized global color map
  FImage.GlobalColorMap.ImportColorMap(FColorMap, FFinalCount);
  FImage.GlobalColorMap.Optimized := True;
end;

procedure TColorMapOptimizer.Optimize;
var
  Total			: integer;
  i, j			: integer;
begin
  // Stop all painters during optimize...
  FImage.PaintStop;
  // ...and prevent any new from starting while we are doing our thing
  FImage.Painters.LockList;
  try

    (*
    **  Process all sub images
    *)
    ProcessImage;

    // Prune histograms and calculate total number of colors
    Total := 0;
    for i := 0 to FHistogramList.Count-1 do
      inc(Total, THistogram(FHistogramList[i]).Prune);

    // Allocate global histogram
    FHistogram.Clear;
    FHistogram.Capacity := Total;

    // Move data pointers from local histograms to global histogram
    for i := 0 to FHistogramList.Count-1 do
      with THistogram(FHistogramList[i]) do
        for j := 0 to Count-1 do
        begin
          ASSERT(POptimizeEntry(List[j])^.Count > 0, 'Unused entry exported from THistogram');
          FHistogram.Add(List[j]);
        end;

    (*
    **  Merge same colors
    *)
    MergeColors;

    (*
    **  Build color map and NewIndex map for final entries
    *)
    MapColors;

    (*
    **  Replace local colormaps with global color map
    *)
    ReplaceColorMaps;

    (*
    **  Process images for each color map
    *)
    for i := 0 to FHistogramList.Count-1 do
      THistogram(FHistogramList[i]).MapImages(FUseTransparency, FNewTransparentColorIndex);

    (*
    **  Delete the frame's old bitmaps and palettes
    *)
    for i := 0 to FImage.Images.Count-1 do
    begin
      FImage.Images[i].HasBitmap := False;
      FImage.Images[i].Palette := 0;
    end;

  finally
    FImage.Painters.UnlockList;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFImage
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFImage.Create;
begin
  inherited Create;
  FImages := TGIFImageList.Create(self);
  FHeader := TGIFHeader.Create(self);
  FPainters := TThreadList.Create;
  FGlobalPalette := 0;
  // Load defaults
  FDrawOptions := GIFImageDefaultDrawOptions;
  ColorReduction := GIFImageDefaultColorReduction;
  FReductionBits := GIFImageDefaultColorReductionBits;
  FDitherMode := GIFImageDefaultDitherMode;
  FCompression := GIFImageDefaultCompression;
  FThreadPriority := GIFImageDefaultThreadPriority;
  FAnimationSpeed := GIFImageDefaultAnimationSpeed;

  FDrawBackgroundColor := clNone;
  IsDrawing := False;
  IsInsideGetPalette := False;
  NewImage;
end;

destructor TGIFImage.Destroy;
var
  i			: integer;
begin
  PaintStop;
  with FPainters.LockList do
    try
      for i := Count-1 downto 0 do
        TGIFPainter(Items[i]).FImage := nil;
    finally
      FPainters.UnLockList;
    end;

  Clear;
  FPainters.Free;
  FImages.Free;
  FHeader.Free;
  inherited Destroy;
end;

procedure TGIFImage.Clear;
begin
  PaintStop;
  FreeBitmap;
  FImages.Clear;
  FHeader.ColorMap.Clear;
  FHeader.Height := 0;
  FHeader.Width := 0;
  FHeader.Prepare;
  Palette := 0;
end;

procedure TGIFImage.NewImage;
begin
  Clear;
end;

function TGIFImage.GetVersion: TGIFVersion;
var
  v			: TGIFVersion;
  i			: integer;
begin
  Result := gvUnknown;
  for i := 0 to FImages.Count-1 do
  begin
    v := FImages[i].Version;
    if (v > Result) then
      Result := v;
    if (v >= high(TGIFVersion)) then
      break;
  end;
end;

function TGIFImage.GetColorResolution: integer;
var
  i			: integer;
begin
  Result := FHeader.ColorResolution;
  for i := 0 to FImages.Count-1 do
    if (FImages[i].ColorResolution > Result) then
      Result := FImages[i].ColorResolution;
end;

function TGIFImage.GetBitsPerPixel: integer;
var
  i			: integer;
begin
  Result := FHeader.BitsPerPixel;
  for i := 0 to FImages.Count-1 do
    if (FImages[i].BitsPerPixel > Result) then
      Result := FImages[i].BitsPerPixel;
end;

function TGIFImage.GetBackgroundColorIndex: BYTE;
begin
  Result := FHeader.BackgroundColorIndex;
end;

procedure TGIFImage.SetBackgroundColorIndex(const Value: BYTE);
begin
  FHeader.BackgroundColorIndex := Value;
end;

function TGIFImage.GetBackgroundColor: TColor;
begin
  Result := FHeader.BackgroundColor;
end;

procedure TGIFImage.SetBackgroundColor(const Value: TColor);
begin
  FHeader.BackgroundColor := Value;
end;

function TGIFImage.GetAspectRatio: BYTE;
begin
  Result := FHeader.AspectRatio;
end;

procedure TGIFImage.SetAspectRatio(const Value: BYTE);
begin
  FHeader.AspectRatio := Value;
end;

procedure TGIFImage.SetDrawOptions(Value: TGIFDrawOptions);
begin
  if (FDrawOptions = Value) then
    exit;

  if (DrawPainter <> nil) then
    DrawPainter.Stop;

  FDrawOptions := Value;
  // Zap all bitmaps
  Pack;
  Changed(self);
end;

procedure TGIFImage.SetAnimationSpeed(Value: integer);
begin
  if (Value < 0) then
    Value := 0
  else if (Value > 1000) then
    Value := 1000;
  if (Value <> FAnimationSpeed) then
  begin
    FAnimationSpeed := Value;
    // Use the FPainters threadlist to protect FDrawPainter from being modified
    // by the thread while we mess with it
    with FPainters.LockList do
      try
        if (FDrawPainter <> nil) then
          FDrawPainter.AnimationSpeed := FAnimationSpeed;
      finally
        // Release the lock on FPainters to let paint thread kill itself
        FPainters.UnLockList;
      end;
  end;
end;

procedure TGIFImage.SetReductionBits(Value: integer);
begin
  if (Value < 3) or (Value > 8) then
    Error(sInvalidBitSize);
  FReductionBits := Value;
end;

procedure TGIFImage.OptimizeColorMap;
var
  ColorMapOptimizer	: TColorMapOptimizer;
begin
  ColorMapOptimizer := TColorMapOptimizer.Create(self);
  try
    ColorMapOptimizer.Optimize;
  finally
    ColorMapOptimizer.Free;
  end;
end;

procedure TGIFImage.Optimize(Options: TGIFOptimizeOptions;
  ColorReduction: TColorReduction; DitherMode: TDitherMode;
  ReductionBits: integer);
var
  i			,
  j			: integer;
  Delay			: integer;
  GCE			: TGIFGraphicControlExtension;
  ThisRect		,
  NextRect		,
  MergeRect		: TRect;
  Prog			,
  MaxProg		: integer;

  function Scan(Buf: PChar; Value: Byte; Count: integer): boolean; assembler;
  asm
    PUSH	EDI
    MOV		EDI, Buf
    MOV		ECX, Count
    MOV		AL, Value
    REPNE	SCASB
    MOV		EAX, False
    JNE		@@1
    MOV		EAX, True
@@1:POP		EDI
  end;

begin
  if (Empty) then
    exit;
  // Stop all painters during optimize...
  PaintStop;
  // ...and prevent any new from starting while we are doing our thing
  FPainters.LockList;
  try
    Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressOptimizing);
    try

      Prog := 0;
      MaxProg := Images.Count*6;

      // Sort color map by usage and remove unused entries
      if (ooColorMap in Options) then
      begin
        // Optimize global color map
        if (GlobalColorMap.Count > 0) then
          GlobalColorMap.Optimize;
        // Optimize local color maps
        for i := 0 to Images.Count-1 do
        begin
          inc(Prog);
          if (Images[i].ColorMap.Count > 0) then
          begin
            Images[i].ColorMap.Optimize;
            Progress(Self, psRunning, MulDiv(Prog, 100, MaxProg), False,
              Rect(0,0,0,0), sProgressOptimizing);
          end;
        end;
      end;

      // Remove passive elements, pass 1
      if (ooCleanup in Options) then
      begin
        // Check for transparency flag without any transparent pixels
        for i := 0 to Images.Count-1 do
        begin
          inc(Prog);
          if (Images[i].Transparent) then
          begin
            if not(Scan(Images[i].Data,
                        Images[i].GraphicControlExtension.TransparentColorIndex,
                        Images[i].DataSize)) then
            begin
              Images[i].GraphicControlExtension.Transparent := False;
              Progress(Self, psRunning, MulDiv(Prog, 100, MaxProg), False,
                Rect(0,0,0,0), sProgressOptimizing);
            end;
          end;
        end;

        // Change redundant disposal modes
        for i := 0 to Images.Count-2 do
        begin
          inc(Prog);
          if (Images[i].GraphicControlExtension <> nil) and
            (Images[i].GraphicControlExtension.Disposal in [dmPrevious, dmBackground]) and
            (not Images[i+1].Transparent) then
          begin
            ThisRect := Images[i].BoundsRect;
            NextRect := Images[i+1].BoundsRect;
            if (not IntersectRect(MergeRect, ThisRect, NextRect)) then
              continue;
            // If the next frame completely covers the current frame,
            // change the disposal mode to dmNone
            if (EqualRect(MergeRect, NextRect)) then
              Images[i].GraphicControlExtension.Disposal := dmNone;
            Progress(Self, psRunning, MulDiv(Prog, 100, MaxProg), False,
              Rect(0,0,0,0), sProgressOptimizing);
          end;
        end;
      end else
        inc(Prog, 2*Images.Count);

      // Merge layers of equal pixels (remove redundant pixels)
      if (ooMerge in Options) then
      begin
        // Merge from last to first to avoid intefering with merge
        for i := Images.Count-1 downto 1 do
        begin
          inc(Prog);
          j := i-1;
          // If the "previous" frames uses dmPrevious disposal mode, we must
          // instead merge with the frame before the previous
          while (j > 0) and
            ((Images[j].GraphicControlExtension <> nil) and
             (Images[j].GraphicControlExtension.Disposal = dmPrevious)) do
            dec(j);
          // Merge
          Images[i].Merge(Images[j]);
          Progress(Self, psRunning, MulDiv(Prog, 100, MaxProg), False,
            Rect(0,0,0,0), sProgressOptimizing);
        end;
      end else
        inc(Prog, Images.Count);

      // Crop transparent areas
      if (ooCrop in Options) then
      begin
        for i := Images.Count-1 downto 0 do
        begin
          inc(Prog);
          if (not Images[i].Empty) and (Images[i].Transparent) then
          begin
            // Remember frames delay in case frame is deleted
            Delay := Images[i].GraphicControlExtension.Delay;
            // Crop
            Images[i].Crop;
            // If the frame was completely transparent we remove it
            if (Images[i].Empty) then
            begin
              // Transfer delay to previous frame in case frame was deleted
              if (i > 0) and (Images[i-1].Transparent) then
                Images[i-1].GraphicControlExtension.Delay :=
                  Images[i-1].GraphicControlExtension.Delay + Delay;
              Images.Delete(i);
            end;
            Progress(Self, psRunning, MulDiv(Prog, 100, MaxProg), False,
              Rect(0,0,0,0), sProgressOptimizing);
          end;
        end;
      end else
        inc(Prog, Images.Count);

      // Remove passive elements, pass 2
      inc(Prog, Images.Count);
      if (ooCleanup in Options) then
      begin
        for i := Images.Count-1 downto 0 do
        begin
          // Remove comments and application extensions
          for j := Images[i].Extensions.Count-1 downto 0 do
            if (Images[i].Extensions[j] is TGIFCommentExtension) or
              (Images[i].Extensions[j] is TGIFTextExtension) or
              (Images[i].Extensions[j] is TGIFUnknownAppExtension) or
              ((Images[i].Extensions[j] is TGIFAppExtNSLoop) and
               ((i > 0) or (Images.Count = 1))) then
              Images[i].Extensions.Delete(j);
          if (Images[i].GraphicControlExtension <> nil) then
          begin
            GCE := Images[i].GraphicControlExtension;
            // Zap GCE if all of the following are true:
            // * No delay or only one image
            // * Not transparent
            // * No prompt
            // * No disposal or only one image
            if ((GCE.Delay = 0) or (Images.Count = 1)) and
              (not GCE.Transparent) and
              (not GCE.UserInput) and
              ((GCE.Disposal in [dmNone, dmNoDisposal]) or (Images.Count = 1)) then
            begin
              GCE.Free;
            end;
          end;
          // Zap frame if it has become empty
          if (Images[i].Empty) and (Images[i].Extensions.Count = 0) then
            Images[i].Free;
        end;
        Progress(Self, psRunning, MulDiv(Prog, 100, MaxProg), False,
          Rect(0,0,0,0), sProgressOptimizing);
      end else

      // Reduce color depth
      if (ooReduceColors in Options) then
      begin
        if (ColorReduction = rmPalette) then
          Error(sInvalidReduction);
        { TODO -oanme -cFeature : Implement ooReduceColors option. }
        // Not implemented!
      end;
    finally
      if ExceptObject = nil then
        i := 100
      else
        i := 0;
      Progress(Self, psEnding, i, False, Rect(0,0,0,0), sProgressOptimizing);
    end;
  finally
    FPainters.UnlockList;
  end;
end;

procedure TGIFImage.Pack;
var
  i			: integer;
begin
  // Zap bitmaps and palettes
  FreeBitmap;
  Palette := 0;
  for i := 0 to FImages.Count-1 do
  begin
    FImages[i].Bitmap := nil;
    FImages[i].Palette := 0;
  end;

  // Only pack if no global colormap and a single image
  if (FHeader.ColorMap.Count > 0) or (FImages.Count <> 1) then
    exit;

  // Copy local colormap to global
  FHeader.ColorMap.Assign(FImages[0].ColorMap);
  // Zap local colormap
  FImages[0].ColorMap.Clear;
end;

procedure TGIFImage.SaveToStream(Stream: TStream);
var
  n			: Integer;
begin
  Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressSaving);
  try
    // Write header
    FHeader.SaveToStream(Stream);
    // Write images
    FImages.SaveToStream(Stream);
    // Write trailer
    with TGIFTrailer.Create(self) do
      try
        SaveToStream(Stream);
      finally
        Free;
      end;
  finally
    if ExceptObject = nil then
      n := 100
    else
      n := 0;
    Progress(Self, psEnding, n, True, Rect(0,0,0,0), sProgressSaving);
  end;
end;

procedure TGIFImage.LoadFromStream(Stream: TStream);
var
  n			: Integer;
  Position		: integer;
begin
  Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressLoading);
  try
    // Zap old image
    Clear;
    Position := Stream.Position;
    try
      // Read header
      FHeader.LoadFromStream(Stream);
      // Read images
      FImages.LoadFromStream(Stream, self);
      // Read trailer
      with TGIFTrailer.Create(self) do
        try
          LoadFromStream(Stream);
        finally
          Free;
        end;
    except
      // Restore stream position in case of error.
      // Not required, but "a nice thing to do"
      Stream.Position := Position;
      raise;
    end;
  finally
    if ExceptObject = nil then
      n := 100
    else
      n := 0;
    Progress(Self, psEnding, n, True, Rect(0,0,0,0), sProgressLoading);
  end;
end;

function TGIFImage.GetBitmap: TBitmap;
begin
  if not(Empty) then
  begin
    Result := FBitmap;
    if (Result <> nil) then
      exit;
    FBitmap := TBitmap.Create;
    Result := FBitmap;
    FBitmap.OnChange := Changed;
    // Use first image as default
    if (Images.Count > 0) then
    begin
      if (Images[0].Width = Width) and (Images[0].Height = Height) then
      begin
        // Use first image as it has same dimensions
        FBitmap.Assign(Images[0].Bitmap);
      end else
      begin
        // Draw first image on bitmap
        FBitmap.Palette := CopyPalette(Palette);
        FBitmap.Height := Height;
        FBitmap.Width := Width;
        Images[0].Draw(FBitmap.Canvas, FBitmap.Canvas.ClipRect, False, False);
      end;
    end;
  end else
    Result := nil
end;

// Create a new (empty) bitmap
function TGIFImage.NewBitmap: TBitmap;
begin
  Result := FBitmap;
  if (Result <> nil) then
    exit;
  FBitmap := TBitmap.Create;
  Result := FBitmap;
  FBitmap.OnChange := Changed;
  // Draw first image on bitmap
  FBitmap.Palette := CopyPalette(Palette);
  FBitmap.Height := Height;
  FBitmap.Width := Width;
end;

procedure TGIFImage.FreeBitmap;
begin
  if (DrawPainter <> nil) then
    DrawPainter.Stop;

  if (FBitmap <> nil) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

function TGIFImage.Add(Source: TPersistent): integer;
var
  Image			: TGIFSubImage;
begin
  Image := nil; // To avoid compiler warning - not needed.
  if (Source is TGraphic) then
  begin
    Image := TGIFSubImage.Create(self);
    try
      Image.Assign(Source);
      // ***FIXME*** Documentation should explain the inconsistency here:
      // TGIFimage does not take ownership of Source after TGIFImage.Add() and
      // therefore does not delete Source.
    except
      Image.Free;
      raise;
    end;
  end else
  if (Source is TGIFSubImage) then
    Image := TGIFSubImage(Source)
  else
    Error(sUnsupportedClass);

  Result := FImages.Add(Image);

  FreeBitmap;
  Changed(self);
end;

function TGIFImage.GetEmpty: Boolean;
begin
  Result := (FImages.Count = 0);
end;

function TGIFImage.GetHeight: Integer;
begin
  Result := FHeader.Height;
end;

function TGIFImage.GetWidth: Integer;
begin
  Result := FHeader.Width;
end;

function TGIFImage.GetIsTransparent: Boolean;
var
  i			: integer;
begin
  Result := False;
  for i := 0 to Images.Count-1 do
    if (Images[i].GraphicControlExtension <> nil) and
      (Images[i].GraphicControlExtension.Transparent) then
    begin
      Result := True;
      exit;
    end;
end;

function TGIFImage.Equals(Graphic: TGraphic): Boolean;
begin
  Result := (Graphic = self);
end;

function TGIFImage.GetPalette: HPALETTE;
begin
  // Check for recursion
  // (TGIFImage.GetPalette->TGIFSubImage.GetPalette->TGIFImage.GetPalette etc...)
  if (IsInsideGetPalette) then
    Error(sNoColorTable);
  IsInsideGetPalette := True;
  try
    Result := 0;
    if (FBitmap <> nil) and (FBitmap.Palette <> 0) then
      // Use bitmaps own palette if possible
      Result := FBitmap.Palette
    else if (FGlobalPalette <> 0) then
      // Or a previously exported global palette
      Result := FGlobalPalette
    else if (DoDither) then
    begin
      // or create a new dither palette
      FGlobalPalette := WebPalette;
      Result := FGlobalPalette;
    end else
    if (FHeader.ColorMap.Count > 0) then
    begin
      // or create a new if first time
      FGlobalPalette := FHeader.ColorMap.ExportPalette;
      Result := FGlobalPalette;
    end else
    if (FImages.Count > 0) then
      // This can cause a recursion if no global palette exist and image[0]
      // hasn't got one either. Checked by the IsInsideGetPalette semaphor.
      Result := FImages[0].Palette;
  finally
    IsInsideGetPalette := False;
  end;
end;

procedure TGIFImage.SetPalette(Value: HPalette);
var
  NeedNewBitmap		: boolean;
begin
  if (Value <> FGlobalPalette) then
  begin
    // Zap old palette
    if (FGlobalPalette <> 0) then
      DeleteObject(FGlobalPalette);

    // Zap bitmap unless new palette is same as bitmaps own
    NeedNewBitmap := (FBitmap <> nil) and (Value <> FBitmap.Palette);

    // Use new palette
    FGlobalPalette := Value;

    if (NeedNewBitmap) then
    begin
      // Need to create new bitmap and repaint
      FreeBitmap;
      PaletteModified := True;
      Changed(Self);
    end;
  end;
end;

// Obsolete
// procedure TGIFImage.Changed(Sender: TObject);
// begin
//  inherited Changed(Sender);
// end;

procedure TGIFImage.SetHeight(Value: Integer);
var
  i			: integer;
begin
  for i := 0 to Images.Count-1 do
    if (Images[i].Top + Images[i].Height > Value) then
      Error(sBadHeight);
  if (Value <> Header.Height) then
  begin
    Header.Height := Value;
    FreeBitmap;
    Changed(self);
  end;
end;

procedure TGIFImage.SetWidth(Value: Integer);
var
  i			: integer;
begin
  for i := 0 to Images.Count-1 do
    if (Images[i].Left + Images[i].Width > Value) then
      Error(sBadWidth);
  if (Value <> Header.Width) then
  begin
    Header.Width := Value;
    FreeBitmap;
    Changed(self);
  end;
end;

procedure TGIFImage.WriteData(Stream: TStream);
begin
  if (GIFImageOptimizeOnStream) then
    Optimize([ooCrop, ooMerge, ooCleanup, ooColorMap, ooReduceColors], rmNone, dmNearest, 8);

  inherited WriteData(Stream);
end;

procedure TGIFImage.AssignTo(Dest: TPersistent);
begin
  if (Dest is TBitmap) then
    Dest.Assign(Bitmap)
  else
    inherited AssignTo(Dest);
end;

{ TODO 1 -oanme -cImprovement : Better handling of TGIFImage.Assign(Empty TBitmap). }
procedure TGIFImage.Assign(Source: TPersistent);
var
  i			: integer;
  Image			: TGIFSubImage;
begin
  if (Source = self) then
    exit;
  if (Source = nil) then
  begin
    Clear;
  end else
  //
  // TGIFImage import
  //
  if (Source is TGIFImage) then
  begin
    Clear;
    // Temporarily copy event handlers to be able to generate progress events
    // during the copy and handle copy errors
    OnProgress := TGIFImage(Source).OnProgress;
    try
      FOnWarning := TGIFImage(Source).OnWarning;
      Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressCopying);
      try
        FHeader.Assign(TGIFImage(Source).Header);
        FThreadPriority := TGIFImage(Source).ThreadPriority;
        FDrawBackgroundColor := TGIFImage(Source).DrawBackgroundColor;
        FDrawOptions := TGIFImage(Source).DrawOptions;
        FColorReduction := TGIFImage(Source).ColorReduction;
        FDitherMode := TGIFImage(Source).DitherMode;

        for i := 0 to TGIFImage(Source).Images.Count-1 do
        begin
          Image := TGIFSubImage.Create(self);
          Image.Assign(TGIFImage(Source).Images[i]);
          Add(Image);
          Progress(Self, psRunning, MulDiv((i+1), 100, TGIFImage(Source).Images.Count),
            False, Rect(0,0,0,0), sProgressCopying);
        end;
      finally
        if ExceptObject = nil then
          i := 100
        else
          i := 0;
        Progress(Self, psEnding, i, False, Rect(0,0,0,0), sProgressCopying);
      end;
    finally
      // Reset event handlers
      FOnWarning := nil;
      OnProgress := nil;
    end;
  end else
  //
  // Import via TGIFSubImage.Assign
  //
  begin
    Clear;
    Image := TGIFSubImage.Create(self);
    try
      Image.Assign(Source);
      Add(Image);
    except
      on E: EConvertError do
      begin
        Image.Free;
        // Unsupported format - fall back to Source.AssignTo
        inherited Assign(Source);
      end;
    else
      // Unknown conversion error
      Image.Free;
      raise;
    end;
  end;
end;

procedure TGIFImage.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
{$IFDEF REGISTER_TGIFIMAGE}
var
  Size			: Longint;
  Buffer		: Pointer;
  Stream		: TMemoryStream;
  Bmp			: TBitmap;
begin
  if (AData = 0) then
    AData := GetClipboardData(AFormat);
  if (AData <> 0) and (AFormat = CF_GIF) then
  begin
    // Get size and pointer to data
    Size := GlobalSize(AData);
    Buffer := GlobalLock(AData);
    try
      Stream := TMemoryStream.Create;
      try
        // Copy data to a stream
        Stream.SetSize(Size);
        Move(Buffer^, Stream.Memory^, Size);
        // Load GIF from stream
        LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
    finally
      GlobalUnlock(AData);
    end;
  end else
  if (AData <> 0) and (AFormat = CF_BITMAP) then
  begin
    // No GIF on clipboard - try loading a bitmap instead
    Bmp := TBitmap.Create;
    try
      Bmp.LoadFromClipboardFormat(AFormat, AData, APalette);
      Assign(Bmp);
    finally
      Bmp.Free;
    end;
  end else
    Error(sUnknownClipboardFormat);
end;
{$ELSE}
begin
  Error(sGIFToClipboard);
end;
{$ENDIF}

procedure TGIFImage.SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
  var APalette: HPALETTE);
{$IFDEF REGISTER_TGIFIMAGE}
var
  Stream		: TMemoryStream;
  Data			: THandle;
  Buffer		: Pointer;
begin
  if (Empty) then
    exit;
  // First store a bitmap version on the clipboard...
  Bitmap.SaveToClipboardFormat(AFormat, AData, APalette);
  // ...then store a GIF
  Stream := TMemoryStream.Create;
  try
    // Save the GIF to a memory stream
    SaveToStream(Stream);
    Stream.Position := 0;
    // Allocate some memory for the GIF data
    Data := GlobalAlloc(HeapAllocFlags, Stream.Size);
    try
      if (Data <> 0) then
      begin
        Buffer := GlobalLock(Data);
        try
          // Copy GIF data from stream memory to clipboard memory
          Move(Stream.Memory^, Buffer^, Stream.Size);
        finally
          GlobalUnlock(Data);
        end;
        // Transfer data to clipboard
        if (SetClipboardData(CF_GIF, Data) = 0) then
          Error(sFailedPaste);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Stream.Free;
  end;
end;
{$ELSE}
begin
  Error(sGIFToClipboard);
end;
{$ENDIF}

function TGIFImage.GetColorMap: TGIFColorMap;
begin
  Result := FHeader.ColorMap;
end;

function TGIFImage.GetDoDither: boolean;
begin
  Result := (goDither in DrawOptions) and
    (((goAutoDither in DrawOptions) and DoAutoDither) or
      not(goAutoDither in DrawOptions));
end;

{$IFDEF VER9x}
procedure TGIFImage.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;
{$ENDIF}

procedure TGIFImage.StopDraw;
{$IFNDEF VER14_PLUS}  // 2001.07.23
var
  Msg			: TMsg;
  ThreadWindow		: HWND;
{$ENDIF}  // 2001.07.23
begin
  repeat
    // Use the FPainters threadlist to protect FDrawPainter from being modified
    // by the thread while we mess with it
    with FPainters.LockList do
      try
        if (FDrawPainter = nil) then
          break;

        // Tell thread to terminate
        FDrawPainter.Stop;

        // No need to wait for "thread" to terminate if running in main thread
        if not(goAsync in FDrawPainter.DrawOptions) then
          break;

      finally
        // Release the lock on FPainters to let paint thread kill itself
        FPainters.UnLockList;
      end;

    // Process Messages to make Synchronize work
    // (Instead of Application.ProcessMessages)
{$IFDEF VER14_PLUS}  // 2001.07.23
    Break;  // 2001.07.23
    Sleep(0); // Yield  // 2001.07.23
{$ELSE}  // 2001.07.23
    ThreadWindow := FindWindow('TThreadWindow', nil);
    while PeekMessage(Msg, ThreadWindow, CM_DESTROYWINDOW, CM_EXECPROC, PM_REMOVE) do
    begin
      if (Msg.Message <> WM_QUIT) then
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end else
      begin
        PostQuitMessage(Msg.WParam);
        exit;
      end;
    end;
    Sleep(0); // Yield
{$ENDIF}  // 2001.07.23
  until (False);
  FreeBitmap;
end;

procedure TGIFImage.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  Canvas		: TCanvas;
  DestRect		: TRect;
{$IFNDEF VER14_PLUS}  // 2001.07.23
  Msg			: TMsg;
  ThreadWindow		: HWND;
{$ENDIF}  // 2001.07.23

  procedure DrawTile(Rect: TRect; Bitmap: TBitmap);
  var
    Tile		: TRect;
  begin
    if (goTile in FDrawOptions) then
    begin
      // Note: This design does not handle transparency correctly!
      Tile.Left := Rect.Left;
      Tile.Right := Tile.Left + Width;
      while (Tile.Left < Rect.Right) do
      begin
        Tile.Top := Rect.Top;
        Tile.Bottom := Tile.Top + Height;
        while (Tile.Top < Rect.Bottom) do
        begin
          ACanvas.StretchDraw(Tile, Bitmap);
          Tile.Top := Tile.Top + Height;
          Tile.Bottom := Tile.Top + Height;
        end;
        Tile.Left := Tile.Left + Width;
        Tile.Right := Tile.Left + Width;
      end;
    end else
      ACanvas.StretchDraw(Rect, Bitmap);
  end;

begin
  // Prevent recursion(s(s(s)))
  if (IsDrawing) or (FImages.Count = 0) then
    exit;

  IsDrawing := True;
  try
    // Copy bitmap to canvas if we are already drawing
    // (or have drawn but are finished)
    if (FImages.Count = 1) or // Only one image
      (not (goAnimate in FDrawOptions)) then // Don't animate
    begin
      FImages[0].Draw(ACanvas, Rect, (goTransparent in FDrawOptions),
        (goTile in FDrawOptions));
      exit;
    end else
    if (FBitmap <> nil) and not(goDirectDraw in FDrawOptions) then
    begin
      DrawTile(Rect, Bitmap);
      exit;
    end;

    // Use the FPainters threadlist to protect FDrawPainter from being modified
    // by the thread while we mess with it
    with FPainters.LockList do
      try
      // If we are already painting on the canvas in goDirectDraw mode
      // and at the same location, just exit and let the painter do
      // its thing when it's ready
      if (FDrawPainter <> nil) and (FDrawPainter.Canvas = ACanvas) and
        EqualRect(FDrawPainter.Rect, Rect) then
        exit;

      // Kill the current paint thread
      StopDraw;

      if not(goDirectDraw in FDrawOptions) then
      begin
        // Create a bitmap to draw on
        NewBitmap;
        Canvas := FBitmap.Canvas;
        DestRect := Canvas.ClipRect;
        // Initialize bitmap canvas with background image
        Canvas.CopyRect(DestRect, ACanvas, Rect);
      end else
      begin
        Canvas := ACanvas;
        DestRect := Rect;
      end;

      // Create new paint thread
      InternalPaint(@FDrawPainter, Canvas, DestRect, FDrawOptions);

      if (FDrawPainter <> nil) then
      begin
        // Launch thread
        FDrawPainter.Start;

        if not(goDirectDraw in FDrawOptions) then
        begin
{$IFNDEF VER14_PLUS}  // 2001.07.23
          ThreadWindow := FindWindow('TThreadWindow', nil);
          // Wait for thread to render first frame
          while (FDrawPainter <> nil) and (not FDrawPainter.Terminated) and
            (not FDrawPainter.Started) do
            // Process Messages to make Synchronize work
            // (Instead of Application.ProcessMessages)
            if PeekMessage(Msg, ThreadWindow, CM_DESTROYWINDOW, CM_EXECPROC, PM_REMOVE) then
            begin
              if (Msg.Message <> WM_QUIT) then
              begin
                TranslateMessage(Msg);
                DispatchMessage(Msg);
              end else
              begin
                PostQuitMessage(Msg.WParam);
                exit;
              end;
            end else
              Sleep(0); // Yield
{$ENDIF}  // 2001.07.23
          // Draw frame to destination
          DrawTile(Rect, Bitmap);
        end;
      end;
    finally
      FPainters.UnLockList;
    end;

  finally
    IsDrawing := False;
  end;
end;

// Internal pain(t) routine used by Draw()
function TGIFImage.InternalPaint(Painter: PGifPainter; ACanvas: TCanvas;
  const Rect: TRect; Options: TGIFDrawOptions): TGIFPainter;
begin
  if (Empty) or (Rect.Left >= Rect.Right) or (Rect.Top >= Rect.Bottom) then
  begin
    Result := nil;
    if (Painter <> nil) then
      Painter^ := Result;
    exit;
  end;

  // Draw in main thread if only one image
  if (Images.Count = 1) then
    Options := Options - [goAsync, goAnimate];

  Result := TGIFPainter.CreateRef(Painter, self, ACanvas, Rect, Options);
  FPainters.Add(Result);
  Result.OnStartPaint := FOnStartPaint;
  Result.OnPaint := FOnPaint;
  Result.OnAfterPaint := FOnAfterPaint;
  Result.OnLoop := FOnLoop;
  Result.OnEndPaint := FOnEndPaint;

  if not(goAsync in Options) then
  begin
    // Run in main thread
    Result.Execute;
    // Note: Painter threads executing in the main thread are freed upon exit
    // from the Execute method, so no need to do it here.
    Result := nil;
    if (Painter <> nil) then
      Painter^ := Result;
  end else
    Result.Priority := FThreadPriority;
end;

function TGIFImage.Paint(ACanvas: TCanvas; const Rect: TRect;
  Options: TGIFDrawOptions): TGIFPainter;
begin
  Result := InternalPaint(nil, ACanvas, Rect, Options);
  if (Result <> nil) then
    // Run in separate thread
    Result.Start;
end;

procedure TGIFImage.PaintStart;
var
  i			: integer;
begin
  with FPainters.LockList do
    try
      for i := 0 to Count-1 do
        TGIFPainter(Items[i]).Start;
    finally
      FPainters.UnLockList;
    end;
end;

procedure TGIFImage.PaintStop;
var
  Ghosts		: integer;
  i			: integer;
{$IFNDEF VER14_PLUS}  // 2001.07.23
  Msg			: TMsg;
  ThreadWindow		: HWND;
{$ENDIF}  // 2001.07.23

{$IFNDEF VER14_PLUS}  // 2001.07.23
  procedure KillThreads;
  var
    i			: integer;
  begin
    with FPainters.LockList do
      try
        for i := Count-1 downto 0 do
          if (goAsync in TGIFPainter(Items[i]).DrawOptions) then
          begin
            TerminateThread(TGIFPainter(Items[i]).Handle, 0);
            Delete(i);
          end;
      finally
        FPainters.UnLockList;
      end;
  end;
{$ENDIF}  // 2001.07.23

begin
  try
    // Loop until all have died
    repeat
      with FPainters.LockList do
        try
          if (Count = 0) then
            exit;

          // Signal painters to terminate
          // Painters will attempt to remove them self from the
          // painter list when they die
          Ghosts := Count;
          for i := Ghosts-1 downto 0 do
          begin
            if not(goAsync in TGIFPainter(Items[i]).DrawOptions) then
              dec(Ghosts);
            TGIFPainter(Items[i]).Stop;
          end;
        finally
          FPainters.UnLockList;
        end;

      // If all painters were synchronous, there's no purpose waiting for them
      // to terminate, because they are running in the main thread.
      if (Ghosts = 0) then
        exit;

      // Process Messages to make TThread.Synchronize work
      // (Instead of Application.ProcessMessages)
{$IFDEF VER14_PLUS}  // 2001.07.23
      Exit;  // 2001.07.23
{$ELSE}  // 2001.07.23
      ThreadWindow := FindWindow('TThreadWindow', nil);
      if (ThreadWindow = 0) then
      begin
        KillThreads;
        exit;
      end;
      while PeekMessage(Msg, ThreadWindow, CM_DESTROYWINDOW, CM_EXECPROC, PM_REMOVE) do
      begin
        if (Msg.Message <> WM_QUIT) then
        begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end else
        begin
          KillThreads;
          exit;
        end;
      end;
      Sleep(0);
{$ENDIF}  // 2001.07.23
    until (False);
  finally
    FreeBitmap;
  end;
end;

procedure TGIFImage.PaintPause;
var
  i			: integer;
begin
  with FPainters.LockList do
    try
      for i := 0 to Count-1 do
        TGIFPainter(Items[i]).Suspend;
    finally
      FPainters.UnLockList;
    end;
end;

procedure TGIFImage.PaintResume;
var
  i			: integer;
begin
  // Implementation is currently same as PaintStart, but don't call PaintStart
  // in case its implementation changes
  with FPainters.LockList do
    try
      for i := 0 to Count-1 do
        TGIFPainter(Items[i]).Start;
    finally
      FPainters.UnLockList;
    end;
end;

procedure TGIFImage.PaintRestart;
var
  i			: integer;
begin
  with FPainters.LockList do
    try
      for i := 0 to Count-1 do
        TGIFPainter(Items[i]).Restart;
    finally
      FPainters.UnLockList;
    end;
end;

procedure TGIFImage.Warning(Sender: TObject; Severity: TGIFSeverity; Message: string);
begin
  if (Assigned(FOnWarning)) then
    FOnWarning(Sender, Severity, Message);
end;

{$IFDEF VER12_PLUS}
  {$IFNDEF VER14_PLUS} // not anymore need for Delphi 6 and up  // 2001.07.23
type
  TDummyThread = class(TThread)
  protected
    procedure Execute; override;
  end;
procedure TDummyThread.Execute;
begin
end;
  {$ENDIF}  // 2001.07.23
{$ENDIF}

var
  DesktopDC: HDC;
{$IFDEF VER12_PLUS}
  {$IFNDEF VER14_PLUS} // not anymore need for Delphi 6 and up  // 2001.07.23
  DummyThread: TThread;
  {$ENDIF}  // 2001.07.23
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
//			Initialization
//
////////////////////////////////////////////////////////////////////////////////

initialization
{$IFDEF REGISTER_TGIFIMAGE}
  TPicture.RegisterFileFormat('GIF', sGIFImageFile, TGIFImage);
  CF_GIF := RegisterClipboardFormat(PChar(sGIFImageFile));
  TPicture.RegisterClipboardFormat(CF_GIF, TGIFImage);
{$ENDIF}
  DesktopDC := GetDC(0);
  try
    PaletteDevice := (GetDeviceCaps(DesktopDC, BITSPIXEL) * GetDeviceCaps(DesktopDC, PLANES) <= 8);
    DoAutoDither := PaletteDevice;
  finally
    ReleaseDC(0, DesktopDC);
  end;

{$IFDEF VER9x}
  // Note: This doesn't return the same palette as the Delphi 3 system palette
  // since the true system palette contains 20 entries and the Delphi 3 system
  // palette only contains 16.
  // For our purpose this doesn't matter since we do not care about the actual
  // colors (or their number) in the palette.
  // Stock objects doesn't have to be deleted.
  SystemPalette16 := GetStockObject(DEFAULT_PALETTE);
{$ENDIF}
{$IFDEF VER12_PLUS}
  // Make sure that at least one thread always exist.
  // This is done to circumvent a race condition bug in Delphi 4.x and later:
  // When threads are deleted and created in rapid succesion, a situation might
  // arise where the thread window is deleted *after* the threads it controls
  // has been created. See the Delphi Bug Lists for more information.
  {$IFNDEF VER14_PLUS} // not anymore need for Delphi 6 and up  // 2001.07.23
  DummyThread := TDummyThread.Create(True);
  {$ENDIF}  // 2001.07.23
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//
//			Finalization
//
////////////////////////////////////////////////////////////////////////////////
finalization
  ExtensionList.Free;
  AppExtensionList.Free;
{$IFNDEF VER9x}
  {$IFDEF REGISTER_TGIFIMAGE}
    TPicture.UnregisterGraphicClass(TGIFImage);
  {$ENDIF}
  {$IFDEF VER100}
  if (pf8BitBitmap <> nil) then
    pf8BitBitmap.Free;
  {$ENDIF}
{$ENDIF}
{$IFDEF VER12_PLUS}
  {$IFNDEF VER14_PLUS} // not anymore need for Delphi 6 and up  // 2001.07.23
  if (DummyThread <> nil) then
    DummyThread.Free;
  {$ENDIF}  // 2001.07.23
{$ENDIF}
end.

