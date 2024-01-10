(*======================================================================*
 | unitExIcon.pas                                                       |
 |                                                                      |
 | Encapsulates Windows Icons & Cursors.                                |
 |                                                                      |
 | For icons In TExIconImage, the memory is the bitmapinfo, followed by |
 | the color and mask bits.                                             |
 |                                                                      |
 | For cursors it is preceded by word x and y hotspots                 |
 |                                                                      |
 | This corresponds with what you find in resources, but not .CUR files |
 |                                                                      |
 | .CUR files look like .ICO files except:                              |
 |                                                                      |
 | 1.  The wType is '2' not '1'                                         |
 |                                                                      |
 | 2.  wPlanes and wBitCount contain the X and Y hotspot.  Because of   |
 |     they can only get the color depth from bColorCount, with its     |
 |     max of 256.                                                      |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ----------------------------------------- |
 | 1.0      10/10/2000  CPWW  Original                                  |
 | 1.01     30/04/2001  CPWW  Cursors working                           |
 | 1.02     17/12/2001  CPWW  Bug in displaying icons/cursor in W98     |
 |                            fixed.                                    |
 *----------------------------------------------------------------------*)

unit unitExIcon;

interface

uses Windows, Classes, SysUtils, Graphics;

type

//=============================================================================
// TExIconImage class - Shared image structure for icons & cursors
// nb. the memory image (and of course, the handle) are for one image only


// TIconHeader is variously called NEWHEADER, ICONDIR and GRPICONDIR in the SDK
TIconHeader = packed record
  wReserved : word;    // Must be 0
  wType  : word;       // 1 for icons, 2 for cursors
  wCount : word;       // Number of components
end;
PIconHeader = ^TIconHeader;

// TResourceDirectory is called RESDIR in the SDK.
TResourceDirectory = packed record
  details : packed record case boolean of
    False : (cursorWidth, cursorHeight : word);
    True : (iconWidth, iconHeight, iconColorCount, iconReserved : BYTE)
  end;
  wPlanes, wBitCount : word;
  lBytesInRes : DWORD;
  wNameOrdinal : word
end;
PResourceDirectory = ^TResourceDirectory;

// TIconDirEntry is called ICONDIRENTRY in the SDK
TIconDirEntry = packed record
  bWidth      : BYTE;     // Width, in pixels, of the image
  bHeight     : BYTE;     // Height, in pixels, of the image
  bColorCount : BYTE;     // Number of colors in image (0 if >=8bpp)
  bReserved   : BYTE;     // Reserved ( must be 0)
  wPlanes     : WORD;     // Color Planes    (X Hotspot for cursors)
  wBitCount   : WORD;     // Bits per pixel  (Y Hotspot for cursors - implies MAX 256 color cursors (!))
  dwBytesInRes : DWORD;   // How many bytes in this resource?
  dwImageOffset : DWORD;  // Where in the file is this image?
end;
PIconDirEntry = ^TIconDirEntry;

//-----------------------------------------------------------------------------
// TExIconImage
//
// Each ExIconCursor can have multiple TExIconImage classes - one per format in
// the ICO file or Icon resource/

TExIconImage = class (TSharedImage)
  FIsIcon : boolean;
  FHandle: HICON;
  FPalette : HPALETTE;
  FMemoryImage: TCustomMemoryStream;
  FGotPalette : boolean;  // Indicates that we've got a the palette from the image data
                          // or that there is no palette (eg. it's not pf1bit ..pf8Bit)

  FWidth, FHeight : Integer;
  FPixelFormat : TPixelFormat;

  procedure HandleNeeded;
  procedure PaletteNeeded;
  procedure ImageNeeded;

  function GetBitmapInfo : PBitmapInfo;
  function GetBitmapInfoHeader : PBitmapInfoHeader;
private
  function GetMemoryImage: TCustomMemoryStream;

protected
  procedure FreeHandle; override;
public
  destructor Destroy; override;

  property Handle : HICON read fHandle;                 // The Icon image handle
  property PaletteHandle : HPALETTE read fPalette;      // The Icon image's palette

  property Width : Integer read FWidth;
  property Height : Integer read FHeight;
  property PixelFormat : TPixelFormat read FPixelFormat;
  property MemoryImage : TCustomMemoryStream read GetMemoryImage;
end;

//-----------------------------------------------------------------------------
// TExIconCursor

TExIconCursor = class (TGraphic)
private
  FImages : array of TExIconImage;
  FCurrentImage : Integer;
  FTransparentColor: TColor;

  function GetHandle: HICON;
  function GetPixelFormat: TPixelFormat;
  procedure SetPixelFormat(const Value: TPixelFormat);
  function GetImageCount: Integer;

  procedure ReleaseImages;
  function GetImage(index: Integer): TExIconImage;
  procedure SetHandle(const Value: HICON);
  procedure AssignFromGraphic (source : TGraphic);
  procedure SetCurrentImage(const Value: Integer);

  procedure HandleNeeded;
  procedure PaletteNeeded;
  procedure ImageNeeded;
  procedure ReadIcon (instance : THandle; stream : TCustomMemoryStream; Size : Integer);
protected
  procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
  function GetEmpty: Boolean; override;
  function GetHeight: Integer; override;
  function GetWidth: Integer; override;
  procedure SetHeight(Value: Integer); override;
  procedure SetWidth(Value: Integer); override;
  procedure SetPalette(Value: HPALETTE); override;
  function GetTransparent : boolean; override;
  function GetPalette : HPALETTE; override;

public
  constructor Create; override;
  destructor Destroy; override;
  procedure LoadFromStream(Stream: TStream); override;
  procedure SaveToStream(Stream: TStream); override;
  procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); override;
  procedure LoadFromResourceName (Instance : THandle; const resName : string);
  procedure LoadFromResourceId (Instance : THandle; ResID : Integer);
  procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE); override;
  procedure Assign (source : TPersistent); override;
  procedure AssignTo (dest : TPersistent); override;
  function Releasehandle : HICON;

  procedure SaveImageToFile (const FileName : string);

  // Save just the current image - SaveToFile saves all the images.


  property Handle: HICON read GetHandle write SetHandle;
  property PixelFormat : TPixelFormat read GetPixelFormat write SetPixelFormat;
  property ImageCount : Integer read GetImageCount;
  property Images [index : Integer] : TExIconImage read GetImage;

  property CurrentImage : Integer read fCurrentImage write SetCurrentImage;
  property TransparentColor : TColor read fTransparentColor write fTransparentColor;
end;

//-----------------------------------------------------------------------------
// TExIcon

TExIcon = class (TExIconCursor)
protected
public
  constructor Create; override;
end;

//-----------------------------------------------------------------------------
// TExCursor

TExCursor = class (TExIconCursor)
private
  function GetHotspot: DWORD;
  procedure SetHotspot(const Value: DWORD);
protected
public
  constructor Create; override;
  property Hotspot : DWORD read GetHotspot write SetHotspot;

// nb.  .CUR file format is not the same as resource stream format !!!!

  procedure LoadFromFile (const FileName : string); override;
  procedure SaveToFile (const FileName : string); override;
end;

function GetPixelFormatNumColors (pf : TPixelFormat) : Integer;
function GetPixelFormatBitCount (pf : TPixelFormat) : Integer;

function CreateMappedBitmap (source : TGraphic; palette : HPALETTE; hiPixelFormat : TPixelFormat; Width, Height : Integer) : TBitmap;
function GetBitmapInfoNumColors (const BI : TBitmapInfoHeader) : Integer;
function GetBitmapInfoPixelFormat (const BI : TBitmapInfoHeader) : TPixelFormat;
procedure GetBitmapInfoSizes (const BI : TBitmapInfoHeader; var InfoHeaderSize, ImageSize : DWORD; iconInfo : boolean);
function GetPixelFormat (graphic : TGraphic) : TPixelFormat;

var
  SystemPalette256 : HPALETTE;  // 256 color 'web' palette.
  SystemPalette2 : HPALETTE;

implementation

uses Clipbrd;

resourceString
  rstInvalidIcon           = 'Invalid Icon or Cursor';
  rstInvalidCursor         = 'Invalid cursor';
  rstInvalidBitmap         = 'Invalid Bitmap';
  rstInvalidPixelFormat    = 'Pixel Format Not Valid for Icons or Cursors';

(*----------------------------------------------------------------------*
 | GetPixelFormatNumColors                                              |
 |                                                                      |
 | Get number of colors for a pixel format.  0 if > pf8bit              |
 *----------------------------------------------------------------------*)
function GetPixelFormatNumColors (pf : TPixelFormat) : Integer;
begin
  case pf of
    pf1Bit : Result := 2;
    pf4Bit : Result := 16;
    pf8Bit : Result := 256;
    else
      Result := 0
  end
end;

(*----------------------------------------------------------------------*
 | GetPixelFormatBitCount                                               |
 |                                                                      |
 | Get number of bits per pixel for a pixel format                      |
 *----------------------------------------------------------------------*)
function GetPixelFormatBitCount (pf : TPixelFormat) : Integer;
begin
  case pf of
    pf1Bit : Result := 1;
    pf4Bit : Result := 4;
    pf8Bit : Result := 8;
    pf15Bit : Result := 16; // 16 bpp RGB.  1 unused, 5 R, 5 G, 5 B
    pf16Bit : Result := 16; // 16 bpp BITFIELDS
    pf24Bit : Result := 24;
    pf32Bit : Result := 32  // Either RGB (8 unused, 8 R, 8 G, 8 B) or 32 bit BITFIELDS
    else
      Result := 0
  end
end;

(*----------------------------------------------------------------------*
 | GetPixelFormat                                                       |
 |                                                                      |
 | Get our pixel format.                                                |
 *----------------------------------------------------------------------*)
function GetPixelFormat (graphic : TGraphic) : TPixelFormat;
begin
  if graphic is TBitmap then
    Result := TBitmap (graphic).PixelFormat
  else
    if graphic is TExIconCursor then
      Result := TExIconCursor (graphic).PixelFormat
    else
      Result := pfDevice
end;

(*----------------------------------------------------------------------------*
 | function GDICheck()                                                        |
 |                                                                            |
 | Check GDI APIs                                                             |
 *----------------------------------------------------------------------------*)
function GDICheck(Value: HGDIOBJ): HGDIOBJ;
begin
  if Value = 0 then
    RaiseLastOSError;
  Result := Value;
end;

(*----------------------------------------------------------------------------*
 | procedure InitializeBitmapInfoHeader ()                                    |
 |                                                                            |
 | Initialize a TBitmapInfoHeader from a DIB or DDB bitmap                    |
 *----------------------------------------------------------------------------*)
procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var BI: TBitmapInfoHeader; PixelFormat : TPixelFormat);
var
  DS: TDIBSection;
  Bytes: Integer;
begin
  DS.dsbmih.biSize := 0;
  Bytes := GetObject(Bitmap, SizeOf(DS), @DS);
  if Bytes = 0 then
    raise EInvalidGraphic.Create (rstInvalidBitmap);

  if (Bytes >= (sizeof(DS.dsbm) + sizeof(DS.dsbmih))) and
     (DS.dsbmih.biSize >= DWORD(sizeof(DS.dsbmih))) then
    BI := DS.dsbmih  // It was a DIB bitmap
  else
  begin              // It was a DDB bitmap
    FillChar(BI, sizeof(BI), 0);
    with BI, DS.dsbm do
    begin
      biSize := SizeOf(BI);
      biWidth := bmWidth;
      biHeight := bmHeight;
    end;
  end;

  if PixelFormat in [pf1Bit..pf8Bit] then
  begin
    BI.biBitCount := GetPixelFormatBitCount (PixelFormat);
    BI.biClrUsed := GetPixelFormatNumColors (PixelFormat)
  end
  else
  begin
    BI.biBitCount := DS.dsbm.bmBitsPixel * DS.dsbm.bmPlanes;
    case DS.dsBm.bmBitsPixel of
      1 : BI.biClrUsed := 2;
      4 : BI.biClrUsed := 16;
      8 : BI.biClrUsed := 256
    end
  end;

  BI.biPlanes := 1;
  if BI.biClrImportant > BI.biClrUsed then
    BI.biClrImportant := BI.biClrUsed;

  BI.biSizeImage := 0;  // SDK sample IconPro always sets biSizeImage to 0.  It
                        // seems to be safer to calculate the size from height * bytes per
                        // scan line.  So we'll do the same...
end;


(*----------------------------------------------------------------------------*
 | function GetBitmapInfoNumColors                                            |
 |                                                                            |
 | Get the number of colors (0, 2..256) of a bitmap header.                   |
 *----------------------------------------------------------------------------*)
function GetBitmapInfoNumColors (const BI : TBitmapInfoHeader) : Integer;
begin
  if BI.biBitCount <= 8 then
    if BI.biClrUsed > 0 then
      result := BI.biClrUsed
    else
      result := 1 shl BI.biBitCount
  else
    result := 0;
end;


(*----------------------------------------------------------------------------*
 | function GetBitmapInfoPixelFormat                                          |
 |                                                                            |
 | Get the pixel format of a bitmap header.                                   |
 *----------------------------------------------------------------------------*)
function GetBitmapInfoPixelFormat (const BI : TBitmapInfoHeader) : TPixelFormat;
begin
  case BI.biBitCount of
    1: result := pf1Bit;
    4: result := pf4Bit;
    8: result := pf8Bit;
   16: case BI.biCompression of
         BI_RGB : result := pf15Bit;
         BI_BITFIELDS: result := pf16Bit;
         else
           raise EInvalidGraphic.Create (rstInvalidPixelFormat);
       end;
   24: result := pf24Bit;
   32: result := pf32Bit;
    else
      raise EInvalidGraphic.Create (rstInvalidPixelFormat);
  end
end;


(*----------------------------------------------------------------------------*
 | procedure GetBitmapInfoSizes                                               |
 |                                                                            |
 | Get the size of the info (incl the colortable), and the bitmap bits        |
 *----------------------------------------------------------------------------*)
procedure GetBitmapInfoSizes (const BI : TBitmapInfoHeader; var InfoHeaderSize, ImageSize : DWORD; iconInfo : boolean);
var
  numColors : Integer;
  height : Integer;
begin
  InfoHeaderSize := SizeOf (TBitmapInfoHeader);

  numColors := GetBitmapInfoNumColors (bi);

  if numColors > 0 then
    Inc (InfoHeaderSize, SizeOf(TRGBQuad) * NumColors)
  else
    if (BI.biCompression and BI_BITFIELDS) <> 0 then
      Inc(InfoHeaderSize, 12);

  height := Abs(BI.biHeight);
  if iconInfo then height := height shr 1;
  ImageSize := BytesPerScanLine(BI.biWidth, BI.biBitCount, 32) * Height
end;

(*----------------------------------------------------------------------------*
 | procedure InternalGetDIBSizes ()                                           |
 |                                                                            |
 | Get size of bitmap header (incl. color table) and bitmap bits.             |
 *----------------------------------------------------------------------------*)
procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: DWORD;
  var ImageSize: DWORD; PixelFormat : TPixelFormat);
var
  BI: TBitmapInfoHeader;
begin
  InitializeBitmapInfoHeader(Bitmap, BI, PixelFormat);
  GetBitmapInfoSizes (BI, InfoHeaderSize, ImageSize, False);
end;

(*----------------------------------------------------------------------------*
 | procedure InternalGetDIB ()                                                |
 |                                                                            |
 | Get bitmap bits.  Note that we *always* call this on a bitmap with the     |
 | required colour depth - ie. we don't use this to do mapping.               |
 |                                                                            |
 | We (therefore) don't use GetDIBits here to get the colour table.           |
 *----------------------------------------------------------------------------*)
function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
  BitmapInfo : PBitmapInfo; var Bits; PixelFormat : TPixelFormat): Boolean;
var
  OldPal: HPALETTE;
  DC: HDC;
begin
  InitializeBitmapInfoHeader(Bitmap, BitmapInfo^.bmiHeader, PixelFormat);
  OldPal := 0;
  DC := CreateCompatibleDC(0);
  try
    if Palette <> 0 then
    begin
      OldPal := SelectPalette(DC, Palette, False);
      RealizePalette(DC);
    end;
    Result := GetDIBits(DC, Bitmap, 0, BitmapInfo^.bmiHeader.biHeight, @Bits, BitmapInfo^, DIB_RGB_COLORS) <> 0;
  finally
    if OldPal <> 0 then SelectPalette(DC, OldPal, False);
    DeleteDC(DC);
  end;
end;

(*----------------------------------------------------------------------------*
 | procedure CreateDIBPalette ()                                              |
 |                                                                            |
 | Create the palette from bitmap info.                                       |
 *----------------------------------------------------------------------------*)
function CreateDIBPalette (const bmi : TBitmapInfo) : HPalette;
var
  lpPal : PLogPalette;
  i : Integer;
  numColors : Integer;
  r : RGBQUAD;
begin
  result := 0;

  NumColors := GetBitmapInfoNumColors (bmi.bmiHeader);

  if NumColors > 0 then
  begin
    if NumColors = 1 then
      result := CopyPalette (SystemPalette2)
    else
    begin
      GetMem (lpPal, sizeof (TLogPalette) + sizeof (TPaletteEntry) * NumColors);
      try
        lpPal^.palVersion    := $300;
        lpPal^.palNumEntries := NumColors;

  {$R-}
        for i := 0 to NumColors -1 do
        begin
          r := bmi.bmiColors [i];
          lpPal^.palPalEntry[i].peRed  := bmi.bmiColors [i].rgbRed;
          lpPal^.palPalEntry[i].peGreen  := bmi.bmiColors[i].rgbGreen;
          lpPal^.palPalEntry[i].peBlue  := bmi.bmiColors[i].rgbBlue;
          lpPal^.palPalEntry[i].peFlags := 0 // not bmi.bmiColors[i].rgbReserved !!
        end;
  {$R+}
        result :=  CreatePalette (lpPal^)
      finally
        FreeMem (lpPal)
      end
    end
  end
end;

(*----------------------------------------------------------------------------*
 | procedure CreateMappedBitmap                                               |
 |                                                                            |
 | Copy a graphic to a DIB bitmap with the specified palette or color         |
 | format, and size.                                                          |
 |                                                                            |
 | If the palette is 0, the returned bitmap's pixelformat is hiPixelFormat    |
 | otherwise the returned bitmap's pixel format is set so it's correct for    |
 | the number of colors in the palette.                                       |
 *----------------------------------------------------------------------------*)
function CreateMappedBitmap (source : TGraphic; palette : HPALETTE; hiPixelFormat : TPixelFormat; Width, Height : Integer) : TBitmap;
var
  colorCount : Integer;
begin
  result := TBitmap.Create;
  result.Width := source.Width;
  result.Height := source.Height;

  if palette <> 0 then
  begin
    colorCount := 0;
    if GetObject (palette, sizeof (colorCount), @colorCount) = 0 then
      RaiseLastOSError;

    case colorCount of
      1..2    : result.PixelFormat := pf1Bit;
      3..16   : result.PixelFormat := pf4Bit;
      17..256 : result.PixelFormat := pf8Bit;
      else
        result.PixelFormat := hiPixelFormat;
    end;

    result.Palette := CopyPalette (palette);

    result.Canvas.StretchDraw (rect (0, 0, Width, Height), source);
  end
  else
  begin
    result.PixelFormat := hiPixelFormat;
    result.Canvas.StretchDraw (rect (0, 0, Width, Height), source);
  end
end;

(*----------------------------------------------------------------------------*
 | procedure MaskBitmapBits                                                   |
 |                                                                            |
 | Kinda like MaskBlt - but without the bugs.  SLOW.  Maybe I'll revisit this |
 | use bitblt instead...                                                      |
 |                                                                            |
 | But see MSDN PRB: Trouble Using DIBSection as a Monochrome Mask            |
 *----------------------------------------------------------------------------*)
procedure MaskBitmapBits (bits : PChar; pixelFormat : TPixelFormat; mask : PChar; width, height : DWORD; palette : HPalette);
var
  bpScanline, maskbpScanline : Integer;
  bitsPerPixel, i, j : Integer;
  maskbp, bitbp : byte;
  maskp, bitp : PChar;
  maskPixel : boolean;
  maskByte: dword;
  maskU : UINT;
  maskColor : byte;
  maskColorByte : byte;
begin
                                       // Get 'black' color index.  This is usually 0
                                       // but some people play jokes...

  if palette <> 0 then
  begin
    maskU := GetNearestPaletteIndex (palette, RGB (0, 0, 0));
    if maskU = CLR_INVALID then
      RaiseLastOSError;

    maskColor := maskU
  end
  else
    maskColor := 0;

  bitsPerPixel := GetPixelFormatBitCount (PixelFormat);
  if bitsPerPixel = 0 then
      raise EInvalidGraphic.Create (rstInvalidPixelFormat);

                                       // Get byte count for mask and bitmap
                                       // scanline.  Can be weird because of padding.

  bpScanline := BytesPerScanLine(width, bitsPerPixel, 32);
  maskbpScanline := BytesPerScanline (width, 1, 32);

  maskByte := $ffffffff;                     // Set constant values for 8bpp masks
  maskColorByte := maskColor;

  for i := 0 to height - 1 do          // Go thru each scanline...
  begin

    maskbp := 0;                       // Bit offset in current mask byte
    bitbp := 0;                        // Bit offset in current bitmap byte
    maskp := mask;                     // Pointer to current mask byte
    bitp := bits;                      // Pointer to current bitmap byte;

    for j := 0 to width - 1 do         // Go thru each pixel
    begin
                                       // Pixel should be masked?
      maskPixel := (byte (maskp^) and ($80 shr maskbp)) <> 0;
      if maskPixel then
      begin
        case bitsPerPixel of
          1, 4, 8 :
            begin
              case bitsPerPixel of           // Calculate bit mask and 'black' color bits
                1 :
                  begin
                    maskByte := $80 shr bitbp;
                    maskColorByte := maskColor shl (7 - bitbp);
                  end;

                4 :
                  begin
                    maskByte := $f0 shr bitbp;
                    maskColorByte := maskColor shl (4 - bitbp)
                  end
              end;
                                             // Apply the mask
              bitp^ := char ((byte (bitp^) and (not maskByte)) or maskColorByte);
            end;

          15, 16 :
            PWORD (bitp)^ := $0000;

          24 :
            begin
              PWORD (bitp)^ := $0000;
              PBYTE (bitp + sizeof (WORD))^ := $00
            end;

          32 :
            PDWORD (bitp)^ := $ffffffff;
        end
      end;

      Inc (maskbp);                    // Next mask bit
      if maskbp = 8 then
      begin
        maskbp := 0;
        Inc (maskp)                    // Next mask byte
      end;

      Inc (bitbp, bitsPerPixel);       // Next bitmap bit(s)
      while bitbp >= 8 do
      begin
        Dec (bitbp, 8);
        Inc (bitp)                     // Next bitmap byte
      end
    end;

    Inc (mask, maskbpScanline);        // Set mask for start of next line
    Inc (bits, bpScanLine)             // Set bits to start of next line
  end
end;

{ TExIconCursor }

(*----------------------------------------------------------------------------*
 | procedure TExIcon.Assign                                                   |
 |                                                                            |
 | Assign an TExIcon from another graphic.                                    |
 |                                                                            |
 | A bit of a compromise this...                                              |
 |                                                                            |
 | ... if source is a TExIcon then all images get replaced by the source      |
 |     images.                                                                |
 |                                                                            |
 | ...  Otherwise only the CurrentImage gets replaced                         |
 *----------------------------------------------------------------------------*)
procedure TExIconCursor.Assign(source: TPersistent);
var
  i : Integer;
  src : TExIconCursor;
  image : TExIconImage;
  data : THandle;
begin
  if source is TExIconCursor then
  begin                                 // Share all images from the source TExIcon
    src := TExIconCursor (source);
    FTransparentColor := src.TransparentColor;

    ReleaseImages;
    SetLength (fImages, src.ImageCount);

    for i := 0 to ImageCount - 1 do
    begin
      src.Images [i].Reference;
      fImages [i] := src.Images [i]
    end;

    fCurrentImage := src.FCurrentImage;
    Changed(Self);
  end
  else
    if source = Nil then                  // Clear the current image.
    begin
      image := TExIconImage.Create;
      image.FIsIcon := Images [FCurrentImage].FIsIcon;
      image.FWidth := Images [FCurrentImage].Width;
      image.FHeight := Images [FCurrentImage].Height;
      image.FPixelFormat := Images [FCurrentImage].PixelFormat;

      Images [fCurrentImage].Release;
      FImages [FCurrentImage] := image;
      image.Reference;
      Changed(Self);
    end
    else
      if source is TGraphic then          // Copy from other graphic (TBitmap, etc)
        AssignFromGraphic (TGraphic (source))
      else
        if source is TClipboard then
        begin
          clipboard.Open;
          try
            Data := GetClipboardData(CF_DIB);
            LoadFromClipboardFormat(CF_DIB, Data, 0);
          finally
            clipboard.Close
          end;
        end
        else
          inherited Assign (source)

end;

(*----------------------------------------------------------------------------*
 | procedure TExIconCursor.AssignFromGraphic                                  |
 |                                                                            |
 | Assign an TExIcon from another graphic, converting it to our pixel format  |
 | and palette.                                                               |
 |                                                                            |
 | Internal use only!                                                         |
 *----------------------------------------------------------------------------*)
procedure TExIconCursor.AssignFromGraphic (source : TGraphic);
var
  src, maskBmp : TBitmap;
  offset, infoHeaderSize, imageSize, maskImageSize : DWORD;
  colorBits, maskBits : PChar;
  image : TExIconImage;
  info : PBitmapInfo;
  maskInfo : PBitmapInfo;
  dc : HDC;
begin
  src := Nil;
  maskBmp := TBitmap.Create;

  try
                                         // Get a bitmap with the required format
    src := CreateMappedBitmap (source, Palette, PixelFormat, Width, height);

    maskBmp.Assign (source);             // Get mask bitmap - White where the transparent color
                                         // occurs - otherwise black.

    if source is TBitmap then
      maskBmp.Mask (TBitmap (source).transparentColor)
    else
      if Source is TExIconCursor then
        maskBmp.Mask (TExIconCursor(source).transparentColor)
      else
        maskBmp.Mask (clBlack);

                                      // Get size for mask bits buffer
    maskImageSize := BytesPerScanLine (Width, 1, 32) * Height;

                                      // Get size for color bits buffer
    InternalGetDibSizes (src.Handle, infoHeaderSize, imageSize, PixelFormat);

                                      // Create a memory stream to assemble the icon image
    image := TExIconImage.Create;
    try
      image.Reference;
      image.FMemoryImage := TMemoryStream.Create;
      image.FIsIcon := Self is TExIcon;

      if image.FIsIcon then
        offset := 0
      else
        offset := sizeof (DWORD);

      image.FMemoryImage.Size := infoHeaderSize + imageSize + maskImageSize + offset;

      info := PBitmapInfo (PChar (image.FMemoryImage.Memory) + offset);
      colorBits := PChar (info) + infoHeaderSize;
      maskBits := colorBits + imageSize;

      InternalGetDib (src.Handle, Palette, info, colorBits^, PixelFormat);
                                       // Get the bitmap header, palette & bits


      maskInfo := nil;
      dc := CreateCompatibleDC (0);
      try
        GetMem (maskInfo, SizeOf (TBitmapInfoHeader) + 2 * SizeOf (RGBQUAD));
                                      // Get mask bits

        with maskInfo^.bmiHeader do  // Set the 1st six members of info header, according
        begin                        // to the docs.

          biSize := SizeOf (TBitmapInfoHeader);
          biWidth := Width;
          biHeight := Height;
          biBitCount := 1;
          biPlanes := 1;
          biCompression := BI_RGB;
        end;

        if GetDIBits (dc, maskBmp.Handle, 0, Height, maskBits, maskInfo^, DIB_RGB_COLORS) = 0 then
          RaiseLastOSError;
      finally
        DeleteDC (dc);
        FreeMem (maskInfo)
      end;

      MaskBitmapBits (colorBits, PixelFormat, maskBits, Width, Height, Palette);

      image.FWidth := info^.bmiHeader.biWidth;
      image.FHeight := info^.bmiHeader.biHeight;

      info^.bmiHeader.biHeight := info^.bmiHeader.biHeight * 2;
                                        // Adjust height for funky icon Height thing.

      image.FPixelFormat := src.PixelFormat;

      image.FGotPalette := False;  // ie.  we need to get it later if required.

      if Self is TExCursor then
        PDWORD (image.FMemoryImage.Memory)^ := TExCursor (Self).HotSpot;

      Images [fCurrentImage].Release;
      fImages [fCurrentImage] := Image;
      Changed (self);
    except
      image.Free;
      raise
    end;
  finally
    maskBmp.Free;
    src.Free
  end
end;

(*----------------------------------------------------------------------------*
 | procedure TExIconCursor.AssignTo                                           |
 |                                                                            |
 | Allow assigning to bitmap                                                  |
 *----------------------------------------------------------------------------*)
procedure TExIconCursor.AssignTo(dest: TPersistent);
var
  bmp : TBitmap;
begin
  if dest is TBitmap then
  begin
    bmp := TBitmap (dest);
    bmp.Assign (nil);           // You gotta do this, otherwise transparency goes nuts!
    bmp.PixelFormat := pf24Bit; // Always assign to 24-bit Bitmap so we don't lose colors

    bmp.Width := Width;
    bmp.Height := Height;

    bmp.Transparent := True;
    bmp.TransparentColor := TransparentColor;
    bmp.Canvas.Brush.Color := TransparentColor;
    bmp.Canvas.FillRect (RECT (0, 0, Width, Height));
    bmp.Canvas.Draw (0, 0, self);
  end
  else
    inherited AssignTo (dest)
end;

(*----------------------------------------------------------------------------*
 | constructor TExIconCursor.Create                                           |
 |                                                                            |
 | Constructor for TExICon                                                    |
 *----------------------------------------------------------------------------*)
constructor TExIconCursor.Create;
begin
  inherited Create;
  FTransparentColor := RGB ($fe, $e6, $f8);
  SetLength (FImages, 1);
  FImages [0] := TExIconImage.Create;
  FImages [0].FIsIcon := self is TExIcon;
  Images [0].Reference;
end;

(*----------------------------------------------------------------------------*
 | destructor TExIconCursor.Destroy                                           |
 |                                                                            |
 | destructor for TExIconCursor                                               |
 *----------------------------------------------------------------------------*)
destructor TExIconCursor.Destroy;
begin
  ReleaseImages;
  inherited Destroy
end;

(*----------------------------------------------------------------------------*
 | procedure TExIconCursor.Draw                                               |
 |                                                                            |
 | We should be able to do HandleNeeded/DrawIconEx, however we don't want to  |
 | call 'HandleNeeded' because of NT bugs, so jump through hoops to draw      |
 | direct from the memory image instead.                                      |
 *----------------------------------------------------------------------------*)
procedure TExIconCursor.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  monoBmp, oldMonoBmp : HBITMAP;
  colorBmp, oldColorBmp : HBITMAP;
  colorDC, monoDC, dc : HDC;
  bitsOffset, bitsSize : DWORD;
  info : PBitmapInfo;
  hdr : PBitmapInfoHeader;
  monoInfo : PBitmapInfo;
  bits : PChar;

begin
  with fImages [fCurrentImage] do
    if Assigned (fMemoryImage) then
    begin
      info := GetBitmapInfo;
      hdr := @info^.bmiHeader;

      colorBmp := 0;
      monoBmp := 0;
      oldColorBmp := 0;
      oldMonoBmp := 0;
      monoDC := 0;
      colorDC := 0;
      monoInfo := Nil;

      dc := GDICheck (GetDC (0));
      try
        hdr^.biHeight := hdr^.biHeight div 2;  // Adjust memory image for funky Icon Height thing.

        GetBitmapInfoSizes (hdr^, bitsOffset, bitsSize, False);

                                                // Create Color Bitmap from Color bits & ColorTable
        colorBmp := GDICheck (CreateDIBitmap (dc, info^.bmiHeader, CBM_INIT, PChar (info) + bitsOffset, info^, DIB_RGB_COLORS));
        colorDC := GDICheck (CreateCompatibleDC (0));
        oldColorBmp := GDICheck (SelectObject(colorDC, colorBmp));

                                                // Create mono bitmap.  For some reason, CreateBitmap
                                                // creates it upside down if you give it the bits - so
                                                // you have to do CreateBitmap followed by SetDIBtes

        GetMem (monoInfo, sizeof (TBitmapInfoHeader) + 2 * sizeof (RGBQUAD));
        Move (hdr^, monoInfo^, sizeof (TBitmapInfoHeader));
        monoInfo^.bmiHeader.biBitCount := 1;
        monoInfo^.bmiHeader.biCompression := 0;
        with PRGBQUAD (PChar (monoInfo) + sizeof (TBitmapInfoHeader) + sizeof (RGBQUAD))^ do
        begin
          rgbRed := $ff;
          rgbGreen := $ff;
          rgbBlue := $ff;
          rgbReserved := 0;
        end;

        monoBmp := GDICheck (CreateBitmap (hdr^.biWidth, hdr^.biHeight, 1, 1, Nil));
        bits := PChar (info) + bitsOffset + bitsSize;
        monoDC := GDICheck (CreateCompatibleDC (0));
        GDICheck (SetDIBits (monoDC, monoBmp, 0, hdr^.biHeight, bits, monoInfo^, DIB_RGB_COLORS));
        oldMonoBmp := GDICheck (SelectObject(monoDC, monoBmp));

                                                // Draw the masked bitmap

        with rect do TransparentStretchBlt (ACanvas.Handle,
                               left, top, right - left, bottom - top,
                               colorDC, 0, 0,
                               hdr^.biWidth, hdr^.biHeight, monoDC, 0, 0);

      finally
        hdr^.biHeight := hdr^.biHeight * 2;

        if oldMonoBmp <> 0 then SelectObject (monoDC, oldMonoBmp);
        if monoDC <> 0 then DeleteDC (monoDC);

        if oldColorBmp <> 0 then SelectObject (colorDC, oldColorBmp);
        if colorDC <> 0 then DeleteDC (colorDC);

        if colorBmp <> 0 then DeleteObject (colorBmp);
        if monoBmp <> 0 then DeleteObject (monoBmp);
        ReleaseDC (0, dc);
        if monoInfo <> Nil then FreeMem (monoInfo)
      end
   end
    else
    begin

    // If you've fed an HICON in directly to the handle property you'll get here.
    // DrawIconEx seems to work - it's CreateIconFromresourceex that blows up...

      if Handle <> 0 then
        with rect do DrawIconEx (ACanvas.Handle, left, top, Handle, right - left, bottom - top, 0, 0, DI_NORMAL)
    end
end;

(*----------------------------------------------------------------------------*
 | function TExIconCursor.GetEmpty                                            |
 |                                                                            |
 | Returns true if the TExIconCursor's current image  has neither a handle or |
 | an image                                                                   |
 *----------------------------------------------------------------------------*)
function TExIconCursor.GetEmpty: Boolean;
begin
  with FImages [fCurrentImage] do
    Result := (FHandle = 0) and (FMemoryImage = nil);
end;

(*----------------------------------------------------------------------------*
 | function TExIconCursor.GetHandle                                           |
 |                                                                            |
 | Returns the current image's icon handle.  Calls HandleNeeded which may not |
 | be reliable under NT.                                                      |
 *----------------------------------------------------------------------------*)
function TExIconCursor.GetHandle: HICON;
begin
  HandleNeeded;
  result := Images [fCurrentImage].Handle
end;

(*----------------------------------------------------------------------------*
 | function TExIconCursor.GetHeight                                           |
 |                                                                            |
 | Returns the current image's height in pixels                               |
 *----------------------------------------------------------------------------*)
function TExIconCursor.GetHeight: Integer;
begin
  result := FImages [fCurrentImage].FHeight;
end;

(*----------------------------------------------------------------------------*
 | function TExIconCursor.GetImage                                            |
 |                                                                            |
 | Get the current image TExIconImage instance                                |
 *----------------------------------------------------------------------------*)
function TExIconCursor.GetImage(index: Integer): TExIconImage;
begin
  result := fImages [index]
end;

(*----------------------------------------------------------------------------*
 | function TExIconCursor.GetImageCount                                       |
 |                                                                            |
 | Get the nuber of images in the current icon or cursor                      |
 *----------------------------------------------------------------------------*)
function TExIconCursor.GetImageCount: Integer;
begin
  result := Length (fImages);
end;

(*----------------------------------------------------------------------------*
 | function TExIconCursor.GetPalette                                          |
 |                                                                            |
 | Get the palette handle for the current image                               |
 *----------------------------------------------------------------------------*)
function TExIconCursor.GetPalette: HPALETTE;
begin
  PaletteNeeded;
  result := FImages [fCurrentImage].fPalette;
end;

(*----------------------------------------------------------------------------*
 | function TExIconCursor.GetPixelFormat : TPixelFormat                       |
 |                                                                            |
 | Get the pixel format for the current image                                 |
 *----------------------------------------------------------------------------*)
function TExIconCursor.GetPixelFormat: TPixelFormat;
begin
  result := FImages [fCurrentImage].fPixelFormat
end;

(*----------------------------------------------------------------------------*
 | function TExIconCursor.GetTransparent : boolean                            |
 |                                                                            |
 | Overrides TGraphic method to always return TRUE                            |
 *----------------------------------------------------------------------------*)
function TExIconCursor.GetTransparent: boolean;
begin
  result := True
end;

(*----------------------------------------------------------------------------*
 | function TExIconCursor.GetWidth : Integer                                  |
 |                                                                            |
 | Returns the current image's width in pixels                                |
 *----------------------------------------------------------------------------*)
function TExIconCursor.GetWidth: Integer;
begin
  result := FImages [fCurrentImage].FWidth;
end;

(*----------------------------------------------------------------------------*
 | procedure TExIconCursor.HandleNeeded                                       |
 |                                                                            |
 | Ensure that an HICON handle exists for the current image.  Don't use this  |
 | unless strictly necessary.  It may bugger up in NT4                        |
 *----------------------------------------------------------------------------*)
procedure TExIconCursor.HandleNeeded;
begin
  FImages [FCurrentImage].HandleNeeded;
end;

(*----------------------------------------------------------------------------*
 | procedure TExIconCursor.ImageNeeded                                        |
 |                                                                            |
 | Ensure that a memory image exists for the current image.                   |
 *----------------------------------------------------------------------------*)
procedure TExIconCursor.ImageNeeded;
begin
  with FImages [FCurrentImage] do ImageNeeded;
end;

(*----------------------------------------------------------------------------*
 | procedure TExIconCursor.LoadFromClipboardFormat                            |
 |                                                                            |
 | Ensure that a memory image exists for the current image.  Affects just the |
 | current image.                                                             |
 *----------------------------------------------------------------------------*)
procedure TExIconCursor.LoadFromClipboardFormat(AFormat: Word;
  AData: THandle; APalette: HPALETTE);
var
  Info : PBItmapInfo;
  image : TExIconImage;
  size : DWORD;
  InfoHeaderSize, ImageSize, monoSize : DWORD;
  mask : PByte;
begin
  size := GlobalSize (AData);
  if (size > 0) and (AFormat = CF_DIB) then
  begin

    image := TExIconImage.Create;
    image.FMemoryImage := TMemoryStream.Create;
    image.Reference;

    try
      info := PBitmapInfo (GlobalLock (AData));
      try
        image.FIsIcon := Images [FCurrentImage].FIsIcon;

        image.FWidth := info^.bmiHeader.biWidth;
        image.FHeight := info^.bmiHeader.biHeight;
        image.FPixelFormat := GetBitmapInfoPixelFormat (info^.bmiHeader);

        GetBitmapInfoSizes (info^.bmiHeader, InfoHeaderSize, ImageSize, False);
        monoSize := image.Width * image.FHeight div 8;

        if size = InfoHeaderSize + ImageSize + monoSize then
          image.FMemoryImage.Write (info^, InfoHeaderSize + ImageSize + monoSize)
        else
        begin
          image.FMemoryImage.Write (info^, InfoHeaderSize + ImageSize);
          GetMem (mask, monoSize);
          try
            FillChar (mask^, monoSize, $00);
            image.FMemoryImage.Write (mask^, monoSize)
          finally
            FreeMem (mask)
          end
        end;
        PBitmapInfo (image.FMemoryImage.Memory)^.bmiHeader.biHeight := info^.bmiHeader.biHeight * 2;
      finally
        GlobalUnlock (AData)
      end
    except
      image.Release;
      raise
    end;

    FImages [FCurrentImage].Release;
    FImages [FCurrentImage] := image
  end
end;

procedure TExIconCursor.LoadFromResourceId(Instance: THandle;
  ResID : Integer);
var
  Stream: TCustomMemoryStream;
begin
  Stream := TResourceStream.CreateFromID(Instance, ResID, RT_ICON);
  try
    ReadIcon(Instance, Stream, Stream.Size);
  finally
    Stream.Free;
  end;
end;

procedure TExIconCursor.LoadFromResourceName(Instance: THandle;
  const resName: string);
var
  Stream: TCustomMemoryStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, RT_GROUP_ICON);
  try
    ReadIcon(Instance, Stream, Stream.Size);
  finally
    Stream.Free;
  end;
end;

(*----------------------------------------------------------------------------*
 | procedure TExIconCursor.LoadFromStream                                     |
 |                                                                            |
 | Load all images from a stream                                              |
 *----------------------------------------------------------------------------*)
procedure TExIconCursor.LoadFromStream(Stream: TStream);
var
  hdr : TIconHeader;
  dirEntry : array of TIconDirEntry;
  i : Integer;
  p : PBitmapInfoHeader;
begin
  Stream.Read (hdr, SizeOf (hdr));

  if (self is TExIcon) <> (hdr.wType = 1) then
    raise EInvalidGraphic.Create (rstInvalidIcon);

  ReleaseImages;  // Get rid of existing images

  SetLength (fImages, hdr.wCount);
  SetLength (dirEntry, hdr.wCount);

                  // Create and initialize the ExIconImage classes and read
                  // the dirEntry structures from the stream.

  for i := 0 to hdr.wCount - 1 do
  begin
    fImages [i] := TExIconImage.Create;
    fImages [i].FIsIcon := self is TExIcon;
    fImages [i].FMemoryImage := TMemoryStream.Create;
    fImages [i].Reference;

    Stream.Read (dirEntry [i], SizeOf (TIconDirEntry));
    fImages [i].FWidth := dirEntry [i].bWidth;
    fImages [i].FHeight := dirEntry [i].bHeight;
  end;

                  // Read the icon images into their Memory streams
  for i := 0 to hdr.wCount - 1 do
  begin

    stream.Seek (dirEntry [i].dwImageOffset, soFromBeginning);

    fImages [i].FMemoryImage.CopyFrom (stream, dirEntry [i].dwBytesInRes);

    p := FImages [i].GetBitmapInfoHeader;
    p^.biSizeImage := 0;

    fImages [i].FPixelFormat := GetBitmapInfoPixelFormat (p^);
  end;

  FCurrentImage := 0;
  Changed(Self);
end;

(*----------------------------------------------------------------------------*
 | procedure TExIconCursor.PaletteNeeded                                      |
 |                                                                            |
 | The palette is needed for the current image                                |
 *----------------------------------------------------------------------------*)
procedure TExIconCursor.PaletteNeeded;
begin
  FImages [FCurrentImage].PaletteNeeded;
end;

(*----------------------------------------------------------------------------*
 | procedure TExIconCursor.ReleaseImages                                      |
 |                                                                            |
 | Release images for the icon.  Internal use only - you must set up at least |
 | one new image after calling it.                                            |
 *----------------------------------------------------------------------------*)
procedure TExIconCursor.ReadIcon(instance : THandle; stream: TCustomMemoryStream;
  Size: Integer);
var
  hdr : TIconHeader;
  resDir : TResourceDirectory;
  i : Integer;
  strm1 : TCustomMemoryStream;
  p : PBitmapInfoHEader;
begin
  stream.read (hdr, SizeOf (hdr));

  if (self is TExIcon) <> (hdr.wType = 1) then
    raise EInvalidGraphic.Create (rstInvalidIcon);

  ReleaseImages;  // Get rid of existing images

  SetLength (fImages, hdr.wCount);

  for i := 0 to hdr.wCount - 1 do
  begin
    stream.read (resDir, SizeOf (resDir));

    strm1 := TResourceStream.CreateFromID (Instance, resDir.wNameOrdinal, RT_ICON);
    try
      fImages [i] := TExIconImage.Create;
      fImages [i].FIsIcon := self is TExIcon;
      fImages [i].FMemoryImage := TMemoryStream.Create;
      fImages [i].Reference;

      if Self is TExIcon then
      begin
        fImages [i].FWidth := resDir.details.iconWidth;
        fImages [i].FHeight := resDir.details.iconHeight
      end
      else
      begin
        fImages [i].FWidth := resDir.details.cursorWidth;
        fImages [i].FHeight := resDir.details.cursorHeight
      end;

      fImages [i].FMemoryImage.CopyFrom (strm1, 0);
      p := FImages [i].GetBitmapInfoHeader;
      p^.biSizeImage := 0;

      fImages [i].FPixelFormat := GetBitmapInfoPixelFormat (p^);
    finally
      strm1.Free
    end
  end;

  FCurrentImage := 0;
  Changed(Self);
end;

function TExIconCursor.ReleaseHandle: HICON;
begin
  HandleNeeded;
  if FImages [fCurrentImage].RefCount > 1 then
    Result := CopyIcon (FImages [fCurrentImage].FHandle) else
  begin
    Result := FImages [fCurrentImage].FHandle;
    FImages [fCurrentImage].fHandle := 0
  end
end;

procedure TExIconCursor.ReleaseImages;
var
  i : Integer;
begin
  for i := 0 to Length (fImages) - 1 do
    fImages [i].Release;

  SetLength (fImages, 0)
end;

(*----------------------------------------------------------------------*
 | TExIconCursor.SaveImageToFile
 |                                                                      |
 *----------------------------------------------------------------------*)
procedure TExIconCursor.SaveImageToFile(const FileName: string);
// Save current image to 'ico' file
var
  hdr : TIconHeader;
  dirEntry : TIconDirEntry;
  image : TExIconImage;
  dirSize : Integer;
  stream : TStream;
begin
  hdr.wReserved := 0;
  if not (self is TExCursor) then
    hdr.wType := 1
  else
    hdr.wType := 2;
  hdr.wCount := 1;

  stream := TFileStream.Create (FileName, fmCreate);
  try
    Stream.Write (hdr, SizeOf (hdr));
    dirSize := sizeof (dirEntry) + sizeof (hdr);

    ImageNeeded;
    image := Images [CurrentImage];

    FillChar (dirEntry, SizeOf (dirEntry), 0);

    dirEntry.bWidth := image.Width;
    dirEntry.bHeight := image.Height;

    case image.PixelFormat of
      pf1Bit  : begin dirEntry.bColorCount :=  2; dirEntry.wBitCount :=  0; end;
      pf4Bit  : begin dirEntry.bColorCount := 16; dirEntry.wBitCount :=  0; end;
      pf8Bit  : begin dirEntry.bColorCount :=  0; dirEntry.wBitCount :=  8; end;
      pf16Bit : begin dirEntry.bColorCount :=  0; dirEntry.wBitCount := 16; end;
      pf24Bit : begin dirEntry.bColorCount :=  0; dirEntry.wBitcount := 24; end;
      pf32Bit : begin dirEntry.bColorCount :=  0; dirEntry.wBitCount := 32; end;
      else
        raise EInvalidGraphic.Create (rstInvalidIcon);
    end;

    if hdr.wType = 2 then
    begin
      dirEntry.wPlanes := LOWORD (TExCursor (Self).Hotspot);
      dirEntry.wBitCount := HIWORD (TExCursor (Self).Hotspot)
    end
    else
      dirEntry.wPlanes := 1;
    dirEntry.dwBytesInRes := image.FMemoryImage.Size;
    if hdr.wType = 2 then
    begin
      image.FMemoryImage.Seek (SizeOf (DWORD), soFromBeginning);
      Dec (dirEntry.dwBytesInRes, SizeOf (DWORD))
    end
    else
      image.FMemoryImage.Seek (0, soFromBeginning);

    dirEntry.dwImageOffset := dirSize;
    Stream.Write (dirEntry, SizeOf (dirEntry));
    Stream.CopyFrom (image.FMemoryImage, image.FMemoryImage.Size - image.FMemoryImage.Position);

  finally
    stream.Free
  end
end;

(*----------------------------------------------------------------------------*
 | procedure TExIconCursor.SaveToClipboardFormat                              |
 |                                                                            |
 | Saves the image on the clipboard as a DDB                                  |
 *----------------------------------------------------------------------------*)
procedure TExIconCursor.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
var
  info : PBitmapInfo;
  InfoHeaderSize, ImageSize, monoSize : DWORD;
  buf : PChar;
begin
  AFormat := CF_DIB;
  ImageNeeded;
  info := Images [fCurrentImage].GetBitmapInfo;
  info^.bmiHeader.biHeight := info^.bmiHeader.biHeight div 2;
  try
    GetBitmapInfoSizes (info^.bmiHeader, InfoHeaderSize, ImageSize, False);
    monoSize := Width * Height div 8;

    AData := GlobalAlloc (GMEM_DDESHARE, InfoHeaderSize + ImageSize + monoSize);
    buf := GlobalLock (AData);
    try
      Move (info^, buf^, InfoHeaderSize + ImageSize + monoSize);
    finally
      GlobalUnlock (AData)
    end;

    APalette := 0;  // Don't need the palette, cause we've copied the DIB
  finally
    info^.bmiHeader.biHeight := info^.bmiHeader.biHeight * 2
  end;
end;

procedure TExIconCursor.SaveToStream(Stream: TStream);
var
  hdr : TIconHeader;
  dirEntry : TIconDirEntry;
  image : TExIconImage;
  i, dirSize, offset : Integer;
  oldCurrentImage : Integer;
begin
  hdr.wReserved := 0;
  if not (self is TExCursor) then
    hdr.wType := 1
  else
    hdr.wType := 2;
  hdr.wCount := ImageCount;

  Stream.Write (hdr, SizeOf (hdr));
  dirSize := ImageCount * sizeof (dirEntry) + sizeof (hdr);

  oldCurrentImage := FCurrentImage;
  try
    offset := 0;
    for i := 0 to ImageCount - 1 do
    begin
      FCurrentImage := i;
      ImageNeeded;
      image := Images [i];

      FillChar (dirEntry, SizeOf (dirEntry), 0);

      dirEntry.bWidth := image.Width;
      dirEntry.bHeight := image.Height;

      case image.PixelFormat of
        pf1Bit  : begin dirEntry.bColorCount :=  2; dirEntry.wBitCount :=  0; end;
        pf4Bit  : begin dirEntry.bColorCount := 16; dirEntry.wBitCount :=  0; end;
        pf8Bit  : begin dirEntry.bColorCount :=  0; dirEntry.wBitCount :=  8; end;
        pf16Bit : begin dirEntry.bColorCount :=  0; dirEntry.wBitCount := 16; end;
        pf24Bit : begin dirEntry.bColorCount :=  0; dirEntry.wBitcount := 24; end;
        pf32Bit : begin dirEntry.bColorCount :=  0; dirEntry.wBitCount := 32; end;
        else
          raise EInvalidGraphic.Create (rstInvalidIcon);
      end;

      dirEntry.wPlanes := 1;
      dirEntry.dwBytesInRes := image.FMemoryImage.Size;
      dirEntry.dwImageOffset := dirSize + offset;

      Stream.Write (dirEntry, SizeOf (dirEntry));
      Inc (offset, dirEntry.dwBytesInRes);
    end
  finally
    FCurrentImage := oldCurrentImage
  end;

  for i := 0 to ImageCount - 1 do
    images [i].FMemoryImage.SaveToStream (Stream);
end;

procedure TExIconCursor.SetCurrentImage(const Value: Integer);
begin
  if fCurrentImage <> value then
  begin
    fCurrentImage := Value;
    Changed (self)
  end
end;

procedure TExIconCursor.SetHandle(const Value: HICON);
var
  iconInfo : TIconInfo;
  BI : TBitmapInfoHeader;
  image : TExIconImage;
begin
  if GetIconInfo (value, iconInfo) then
  try
    image := TExIconImage.Create;
    try
      InitializeBitmapInfoHeader (iconInfo.hbmColor, BI, pfDevice);
      image.FIsIcon := self is TExIcon;
      image.FWidth := BI.biWidth;
      image.FHeight := BI.biHeight;
      image.FPixelFormat := GetBitmapInfoPixelFormat (BI);
    except
      image.Free;
      raise
    end;

    image.FHandle := Value;

    Images [fCurrentImage].Release;
    fImages [fCurrentImage] := image;
    image.Reference;
    Changed(Self)
  finally
    DeleteObject (iconInfo.hbmMask);
    DeleteObject (iconInfo.hbmColor)
  end
  else
    RaiseLastOSError;
end;

procedure TExIconCursor.SetHeight(Value: Integer);
begin
  if Value = Height then Exit;
  Images [FCurrentImage].FHeight := Value;
  AssignFromGraphic (Self);
end;

(*----------------------------------------------------------------------------*
 | procedure TExIconCursor.SetPalette                                         |
 |                                                                            |
 | Modify the icon so it uses a new palette (with maybe a differnt color      |
 | count, hence pixel format...                                               |
 *----------------------------------------------------------------------------*)
procedure TExIconCursor.SetPalette(Value: HPALETTE);
var
  colorCount : DWORD;
  newPixelFormat : TPixelFormat;
begin
  newPixelFormat := pfDevice;
  colorCount := 0;
  if GetObject (Value, sizeof (colorCount), @colorCount) = 0 then
    RaiseLastOSError;

  case colorCount of
    1..2    : newPixelFormat := pf1Bit;
    3..16   : newPixelFormat := pf4Bit;
    17..256 : newPixelFormat := pf8Bit;
  end;

  if FImages [FCurrentImage].FPalette <> 0 then
    DeleteObject (FImages [FCurrentImage].FPalette);

  if newPixelFormat <> pfDevice then
  begin
    FImages [FCurrentImage].FPixelFormat := newPixelFormat;

    FImages [FCurrentImage].FPalette := CopyPalette (Value);
    FImages [FCurrentImage].FGotPalette := FImages [FCurrentImage].FPalette <> 0;
    AssignFromGraphic (Self);
  end
  else
  begin
    FImages [FCurrentImage].FPalette := 0;
    FImages [FCurrentImage].FGotPalette := True
  end
end;

(*----------------------------------------------------------------------------*
 | procedure TExIconCursor.SetPixelFormat                                     |
 |                                                                            |
 | Modify the icon so it uses a new pixel format.  If this pixel format has   |
 | <= 256 colours, apply an appropriate palette.  Could modify this to use    |
 | sophisticated color reduction, but at the moment it uses the 'default'     |
 | 16 color palete, and the 'netscape' 256 color one.                         |
 *----------------------------------------------------------------------------*)
procedure TExIconCursor.SetPixelFormat(const Value: TPixelFormat);
var
  newPalette : HPALETTE;
begin
  if value = PixelFormat then Exit;

  case value of
    pf1Bit : newPalette := SystemPalette2;
    pf4Bit : newPalette := SystemPalette16;
    pf8Bit : newPalette := SystemPalette256;
    else
      newPalette := 0
  end;

  FImages [FCurrentImage].FPixelFormat := Value;

  if FImages [FCurrentImage].FPalette <> 0 then
    DeleteObject (FImages [FCurrentImage].FPalette);

  if newPalette <> 0 then
  begin
    FImages [FCurrentImage].FPalette := CopyPalette (newPalette);
    FImages [FCurrentImage].FGotPalette := FImages [FCurrentImage].FPalette <> 0;
  end
  else
  begin
    FImages [FCurrentImage].FPalette := 0;
    FImages [FCurrentImage].FGotPalette := True
  end;

  AssignFromGraphic (self)
end;

procedure TExIconCursor.SetWidth (Value: Integer);
begin
  if Value = Width then Exit;

  Images [FCurrentImage].FWidth := Value;
  AssignFromGraphic (Self);
end;

{ TExIconImage }

destructor TExIconImage.Destroy;
begin
  FMemoryImage.Free;
  inherited                     // Which calls FreeHandle if necessary
end;

procedure TExIconImage.FreeHandle;
begin
  if FHandle <> 0 then
    DestroyIcon(FHandle);

  if FPalette <> 0 then
    DeleteObject (FPalette);
  FGotPalette := False;
  FPalette := 0;
  FHandle := 0;
end;

function TExIconImage.GetBitmapInfo: PBitmapInfo;
begin
  if Assigned (FMemoryImage) then
    if FIsIcon then
      result := PBitmapInfo (FMemoryImage.Memory)
    else
      result := PBitmapInfo (PChar (FMemoryImage.Memory) + sizeof (DWORD))
  else
    result := Nil
end;

function TExIconImage.GetBitmapInfoHeader: PBitmapInfoHeader;
begin
  result := PBitmapInfoHeader (GetBitmapInfo)
end;

function TExIconImage.GetMemoryImage: TCustomMemoryStream;
begin
  ImageNeeded;
  result := FMemoryImage
end;

(*----------------------------------------------------------------------*
 | TExIconImage.HandleNeeded                                            |
 |                                                                      |
 | In general, call this as little as possible.  I don't call it any-   |
 | where in this code - I draw the bitmaps directly, rather than using  |
 | DrawIconEx, etc.                                                     |
 |                                                                      |
 | CreateIconFromResourceEx is very unreliable with icons > 16 colours  |
 *----------------------------------------------------------------------*)
procedure TExIconImage.HandleNeeded;
var
  info : PBitmapInfoHeader;
  buff : PByte;
begin
  if Handle <> 0 then exit;
  if FMemoryImage = Nil then exit;

  if fPalette <> 0 then
  begin
    DeleteObject (fPalette);
    fPalette := 0;
    fGotPalette := False;
  end;

  if FMemoryImage.Size > sizeof (TBitmapInfoHeader) + 4 then
  begin
    info := GetBitmapInfoHeader;

// Aaaagh.  I don't believe I'm doing this.  For some reason you cant use 'FMemoryImage.Memory'
// directly in CreateIconFromResourceEx.  You have to copy it to a (GMEM_MOVEABLE) buffer first.
//
// And they call NT an operating system!

    GetMem (buff, FMemoryImage.Size);
    try
     FMemoryImage.Seek (0, soFromBeginning);
     Move (FMemoryImage.Memory^, buff^, FMemoryImage.Size);

      FHandle := CreateIconFromResourceEx (buff, FMemoryImage.Size, FisIcon, $00030000, info^.biWidth, info^.biHeight div 2, LR_DEFAULTCOLOR);
    finally
      FreeMem (Buff)
    end;

    if FHandle = 0 then raise
      EInvalidGraphic.Create (rstInvalidIcon);

    FWidth := info^.biWidth;
    FHeight := info^.biHeight div 2;
    FPixelFormat := GetBitmapInfoPixelFormat (info^);

    if info^.biBitCount <= 8 then
      FPalette := CreateDIBPalette (PBitmapInfo (info)^);

    fGotPalette := FPalette <> 0;
  end
end;

(*----------------------------------------------------------------------*
 | TExIconImage.ImageNeeded
 |                                                                      |
 *----------------------------------------------------------------------*)
procedure TExIconImage.ImageNeeded;
var
  Image: TMemoryStream;
  IconInfo: TIconInfo;
  MonoInfoSize, ColorInfoSize: DWORD;
  MonoBitsSize, ColorBitsSize: DWORD;
  MonoInfo, MonoBits, ColorInfo, ColorBits: Pointer;
begin
  if FMemoryImage <> nil then Exit;
  if FHandle = 0 then
    raise EInvalidGraphic.Create (rstInvalidIcon);

  Image := TMemoryStream.Create;
  try
    GetIconInfo(Handle, IconInfo);
    try
      InternalGetDIBSizes(IconInfo.hbmMask, MonoInfoSize, MonoBitsSize, pf1Bit);
      if IconInfo.hbmColor <> 0 then
        InternalGetDIBSizes(IconInfo.hbmColor, ColorInfoSize, ColorBitsSize, PixelFormat);

      MonoInfo := nil;
      MonoBits := nil;
      ColorInfo := nil;
      ColorBits := nil;
      try
        MonoInfo := AllocMem(MonoInfoSize);
        MonoBits := AllocMem(MonoBitsSize);
        InternalGetDIB(IconInfo.hbmMask, 0, PBitmapInfo (MonoInfo), MonoBits^, pf1Bit);

        if IconInfo.hbmColor <> 0 then
        begin
          ColorInfo := AllocMem(ColorInfoSize);
          ColorBits := AllocMem(ColorBitsSize);

          InternalGetDIB(IconInfo.hbmColor, FPalette, PBitmapInfo (ColorInfo), ColorBits^, PixelFormat);
          with PBitmapInfoHeader(ColorInfo)^ do
            Inc(biHeight, biHeight); { color height includes mono bits }
        end;

        if (not FIsIcon) then
        begin
          Image.Write (IconInfo.xHotspot, SizeOf (iconInfo.xHotspot));
          Image.Write (IconInfo.yHotspot, SizeOf (iconInfo.yHotspot))
        end;

        if IconInfo.hbmColor <> 0 then
        begin
          Image.Write(ColorInfo^, ColorInfoSize);
          Image.Write(ColorBits^, ColorBitsSize)
        end
        else
          Image.Write(MonoInfo^, MonoInfoSize);

        Image.Write(MonoBits^, MonoBitsSize);
      finally
        FreeMem(ColorInfo, ColorInfoSize);
        FreeMem(ColorBits, ColorBitsSize);
        FreeMem(MonoInfo, MonoInfoSize);
        FreeMem(MonoBits, MonoBitsSize);
      end;
    finally
      if IconInfo.hbmColor <> 0 then
        DeleteObject(IconInfo.hbmColor);
      DeleteObject(IconInfo.hbmMask);
    end
  except
    Image.Free;
    raise;
  end;
  FMemoryImage := Image
end;

(*----------------------------------------------------------------------*
 | TExIconImage.PaletteNeeded
 |                                                                      |
 *----------------------------------------------------------------------*)
procedure TExIconImage.PaletteNeeded;
var
  info : PBitmapInfoHeader;
begin
  if fGotPalette then Exit;
  if fMemoryImage = Nil then Exit;

  info := GetBitmapInfoHeader;

  if fPixelFormat in [pf1Bit..pf8Bit] then
    FPalette := CreateDIBPalette (PBitmapInfo (info)^);

  fGotPalette := True;
end;

{ TExCursor }

(*----------------------------------------------------------------------*
 | TExCursor.Create
 |                                                                      |
 *----------------------------------------------------------------------*)
constructor TExCursor.Create;
begin
  inherited;

  with FImages [0] do
  begin
    fWidth := GetSystemMetrics (SM_CXCURSOR);
    fHeight := GetSystemMetrics (SM_CYCURSOR);
    fPixelFormat := pf1Bit
  end
end;

(*----------------------------------------------------------------------*
 | TExCursor.GetHotspot
 |                                                                      |
 *----------------------------------------------------------------------*)
function TExCursor.GetHotspot: DWORD;
begin
  ImageNeeded;
  Result := PDWORD (Images [fCurrentImage].FMemoryImage.Memory)^
end;

(*----------------------------------------------------------------------*
 | TExCursor.SetHotspot
 |                                                                      |
 *----------------------------------------------------------------------*)
procedure TExCursor.LoadFromFile(const FileName: string);
var
  hdr : TIconHeader;
  dirEntry : array of TIconDirEntry;
  i : Integer;
  p : PBitmapInfoHeader;
  stream : TFileStream;
  hotspot : DWORD;
begin
  stream := TFileStream.Create (FileName, fmOpenRead or fmShareDenyWrite);
  try
    Stream.Read (hdr, SizeOf (hdr));

    if hdr.wType <> 2 then
      raise EInvalidGraphic.Create (rstInvalidCursor);

    ReleaseImages;  // Get rid of existing images

    SetLength (fImages, hdr.wCount);
    SetLength (dirEntry, hdr.wCount);

                    // Create and initialize the ExIconImage classes and read
                    // the dirEntry structures from the stream.

    for i := 0 to hdr.wCount - 1 do
    begin
      fImages [i] := TExIconImage.Create;
      fImages [i].FIsIcon := False;
      fImages [i].FMemoryImage := TMemoryStream.Create;
      fImages [i].Reference;

      Stream.Read (dirEntry [i], SizeOf (TIconDirEntry));
      fImages [i].FWidth := dirEntry [i].bWidth;
      fImages [i].FHeight := dirEntry [i].bHeight;
    end;

                    // Read the icon images into their Memory streams
    for i := 0 to hdr.wCount - 1 do
    begin
      hotspot := MAKELONG (dirEntry [i].wPlanes, dirEntry [i].wBitCount);

      stream.Seek (dirEntry [i].dwImageOffset, soFromBeginning);

      fImages [i].FMemoryImage.Write (hotspot, SizeOf (hotspot));
      fImages [i].FMemoryImage.CopyFrom (stream, dirEntry [i].dwBytesInRes);

      p := FImages [i].GetBitmapInfoHeader;
      p^.biSizeImage := 0;

      fImages [i].FPixelFormat := GetBitmapInfoPixelFormat (p^);
    end;

    FCurrentImage := 0;
    Changed(Self)
  finally
    stream.Free
  end
end;

procedure TExCursor.SaveToFile(const FileName: string);
var
  hdr : TIconHeader;
  dirEntry : TIconDirEntry;
  image : TExIconImage;
  i, dirSize, offset : Integer;
  oldCurrentImage : Integer;
  stream : TFileStream;
begin
  stream := TFileStream.Create (FileName, fmCreate);
  try
    hdr.wReserved := 0;
    hdr.wType := 2;
    hdr.wCount := ImageCount;

    Stream.Write (hdr, SizeOf (hdr));
    dirSize := ImageCount * sizeof (dirEntry) + sizeof (hdr);

    oldCurrentImage := FCurrentImage;
    try
      offset := 0;
      for i := 0 to ImageCount - 1 do
      begin
        FCurrentImage := i;
        ImageNeeded;
        image := Images [i];

        FillChar (dirEntry, SizeOf (dirEntry), 0);

        dirEntry.bWidth := image.Width;
        dirEntry.bHeight := image.Height;

        case image.PixelFormat of
          pf1Bit  : dirEntry.bColorCount :=  2;
          pf4Bit  : dirEntry.bColorCount := 16;
          pf8Bit  : dirEntry.bColorCount :=  0;
          pf16Bit : dirEntry.bColorCount :=  0;
          pf24Bit : dirEntry.bColorCount :=  0;
          pf32Bit : dirEntry.bColorCount :=  0;
          else
            raise EInvalidGraphic.Create (rstInvalidIcon);
        end;

        dirEntry.wPlanes   := LOWORD (Hotspot);
        dirEntry.wBitCount := HIWORD (Hotspot);

        dirEntry.dwBytesInRes := image.FMemoryImage.Size - SizeOf (DWORD);
        dirEntry.dwImageOffset := dirSize + offset;

        Stream.Write (dirEntry, SizeOf (dirEntry));
        Inc (offset, dirEntry.dwBytesInRes);
      end
    finally
      FCurrentImage := oldCurrentImage
    end;

    for i := 0 to ImageCount - 1 do
    begin
      fImages [i].FMemoryImage.Seek (SizeOf (DWORD), soFromBeginning);
      Stream.CopyFrom (images [i].FMemoryImage, images [i].FMemoryImage.Size - images [i].fMemoryImage.Position);
    end
  finally
    stream.Free
  end
end;

(*----------------------------------------------------------------------*
 | TExCursor.SetHotspot                                                 |
 |                                                                      |
 | Set the cursor's hotspot                                             |
 *----------------------------------------------------------------------*)
procedure TExCursor.SetHotspot(const Value: DWORD);
begin
  ImageNeeded;
  PDWORD (images [fCurrentImage].fMemoryImage.memory)^ := Value;
end;

{ TExIcon }

(*----------------------------------------------------------------------*
 | TExIcon.Create
 |                                                                      |
 *----------------------------------------------------------------------*)
constructor TExIcon.Create;
begin
  inherited;
  with FImages [0] do
  begin
    fWidth := GetSystemMetrics (SM_CXICON);
    fHeight := GetSystemMetrics (SM_CYICON);
    fPixelFormat := pf4Bit
  end
end;

(*----------------------------------------------------------------------*
 | WebPalette
 |                                                                      |
 *----------------------------------------------------------------------*)
function WebPalette: HPalette;
type
  TLogWebPalette	= packed record
    palVersion		: word;
    palNumEntries	: word;
    PalEntries		: array [0..5,0..5,0..5] of TPaletteEntry;
    MonoEntries         : array [0..23] of TPaletteEntry;
    StdEntries          : array [0..15] of TPaletteEntry;
  end;
var
  r, g, b		: byte;
  LogWebPalette		: TLogWebPalette;
  LogPalette		: TLogpalette absolute LogWebPalette; // Stupid typecast
begin
  with LogWebPalette do
  begin
    GetPaletteEntries (SystemPalette16, 0, 16, StdEntries);
    palVersion:= $0300;
    palNumEntries:= 256;

    g := 10;
    for r := 0 to 23 do
    begin
      MonoEntries [r].peRed := g;
      MonoEntries [r].peGreen := g;
      MonoEntries [r].peBlue := g;
      MonoEntries [r].peFlags := 0;
      Inc (g, 10)
    end;

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

(*----------------------------------------------------------------------*
 | Create2ColorPalette
 |                                                                      |
 *----------------------------------------------------------------------*)
function Create2ColorPalette : HPALETTE;
const
  palColors2 : array [0..1] of TColor = ($000000, $ffffff);
var
  logPalette : PLogPalette;
  i, c : Integer;

begin
  GetMem (logPalette, sizeof (logPalette) + 2 * sizeof (PALETTEENTRY));

  try
    logPalette^.palVersion := $300;
    logPalette^.palNumEntries := 2;
{$R-}
    for i := 0 to 1 do
      with logPalette^.palPalEntry [i] do
      begin
        c := palColors2 [i];

        peRed := c and $ff;
        peGreen := c shr 8 and $ff;
        peBlue :=  c shr 16 and $ff
      end;
{$R+}
    result := CreatePalette (logPalette^);
  finally
    FreeMem (logPalette)
  end
end;

initialization
  SystemPalette256 := WebPalette;
  SystemPalette2 := Create2ColorPalette;
finalization
  DeleteObject (SystemPalette2);
  DeleteObject (SystemPalette256);
end.
