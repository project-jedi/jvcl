(*======================================================================*
 | unitResourceGraphics                                                 |
 |                                                                      |
 | Encapsulates graphics in resources (icon, cursor, bitmap)            |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      05/01/2001  CPWW  Original                                  |
 *======================================================================*)

unit unitResourceGraphics;

interface

uses Windows, Classes, SysUtils, unitResourceDetails, graphics, unitExIcon, gifimage;

type

//------------------------------------------------------------------------
// Base class

  TGraphicsResourceDetails = class (TResourceDetails)
  protected
    function GetHeight: Integer; virtual; abstract;
    function GetPixelFormat: TPixelFormat; virtual; abstract;
    function GetWidth: Integer; virtual; abstract;
  public
    procedure GetImage (picture : TPicture); virtual; abstract;
    procedure SetImage (image : TPicture); virtual;

    property Width : Integer read GetWidth;
    property Height : Integer read GetHeight;
    property PixelFormat : TPixelFormat read GetPixelFormat;
  end;

  TGraphicsResourceDetailsClass = class of TGraphicsResourceDetails;

//------------------------------------------------------------------------
// Bitmap resource details class

  TBitmapResourceDetails = class (TGraphicsResourceDetails)
  protected
    function GetHeight: Integer; override;
    function GetPixelFormat: TPixelFormat; override;
    function GetWidth: Integer; override;
    procedure InitNew; override;
    procedure InternalGetImage (s : TStream; picture : TPicture);
    procedure InternalSetImage (s : TStream; image : TPicture);

  public
    class function GetBaseType : string; override;
    procedure GetImage (picture : TPicture); override;
    procedure SetImage (image : TPicture); override;
  end;

//------------------------------------------------------------------------
// DIB resource details class
//
// Same as RT_BITMAP resources, but they have a TBitmapFileHeader at the start
// of the resource, before the TBitmapInfoHeader.  See
// \program files\Microsoft Office\office\1033\outlibr.dll

  TDIBResourceDetails = class (TBitmapResourceDetails)
  protected
    class function SupportsData (Size : Integer; data : Pointer) : Boolean; override;
    procedure InitNew; override;
  public
    class function GetBaseType : string; override;
    procedure GetImage (picture : TPicture); override;
    procedure SetImage (image : TPicture); override;
  end;

  TIconCursorResourceDetails = class;

//------------------------------------------------------------------------
// Icon / Cursor group resource details class

  TIconCursorGroupResourceDetails = class (TResourceDetails)
  private
    fDeleting : Boolean;
    function GetResourceCount: Integer;
    function GetResourceDetails(idx: Integer): TIconCursorResourceDetails;
  protected
    procedure InitNew; override;
  public
    procedure GetImage (picture : TPicture);
    property ResourceCount : Integer read GetResourceCount;
    property ResourceDetails [idx : Integer] : TIconCursorResourceDetails read GetResourceDetails;
    function Contains (details : TIconCursorResourceDetails) : Boolean;
    procedure RemoveFromGroup (details : TIconCursorResourceDetails);
    procedure AddToGroup (details : TIconCursorResourceDetails);
    procedure LoadImage (const FileName : string);
    procedure BeforeDelete; override;
  end;

//------------------------------------------------------------------------
// Icon group resource details class

  TIconGroupResourceDetails = class (TIconCursorGroupResourceDetails)
  public
    class function GetBaseType : string; override;
  end;

//------------------------------------------------------------------------
// Cursor group resource details class

  TCursorGroupResourceDetails = class (TIconCursorGroupResourceDetails)
  public
    class function GetBaseType : string; override;
  end;

//------------------------------------------------------------------------
// Icon / Cursor resource details class

  TIconCursorResourceDetails = class (TGraphicsResourceDetails)
  protected
    function GetHeight: Integer; override;
    function GetPixelFormat: TPixelFormat; override;
    function GetWidth: Integer; override;
  protected
    procedure InitNew; override;
  public
    procedure BeforeDelete; override;
    procedure GetImage (picture : TPicture); override;
    procedure SetImage (image : TPicture); override;
    property Width : Integer read GetWidth;
    property Height : Integer read GetHeight;
    property PixelFormat : TPixelFormat read GetPixelFormat;
  end;

//------------------------------------------------------------------------
// Icon resource details class

  TIconResourceDetails = class (TIconCursorResourceDetails)
  public
    class function GetBaseType : string; override;
  end;

//------------------------------------------------------------------------
// Cursor resource details class

  TCursorResourceDetails = class (TIconCursorResourceDetails)
  protected
  public
    class function GetBaseType : string; override;
  end;

const
  DefaultIconCursorWidth : Integer = 32;
  DefaultIconCursorHeight : Integer = 32;
  DefaultIconCursorPixelFormat : TPixelFormat = pf4Bit;
  DefaultCursorHotspot : DWord = $00100010;

  DefaultBitmapWidth : Integer = 128;
  DefaultBitmapHeight : Integer = 96;
  DefaultBitmapPixelFormat : TPixelFormat = pf24Bit;

implementation

type

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

resourcestring
  rstCursors = 'Cursors';
  rstIcons = 'Icons';

{ TBitmapResourceDetails }

(*----------------------------------------------------------------------*
 | TBitmapResourceDetails.GetBaseType                                   |
 *----------------------------------------------------------------------*)
class function TBitmapResourceDetails.GetBaseType: string;
begin
  result := IntToStr (Integer (RT_BITMAP));
end;

(*----------------------------------------------------------------------*
 | TBitmapResourceDetails.GetHeight                                     |
 *----------------------------------------------------------------------*)
function TBitmapResourceDetails.GetHeight: Integer;
begin
  result := PBitmapInfoHeader (data.Memory)^.biHeight
end;

(*----------------------------------------------------------------------*
 | TBitmapResourceDetails.GetImage                                      |
 *----------------------------------------------------------------------*)
procedure TBitmapResourceDetails.GetImage(picture: TPicture);
var
  s : TMemoryStream;
  hdr : TBitmapFileHeader;
begin
  s := TMemoryStream.Create;
  try
    hdr.bfType :=$4D42;         // TBitmap.LoadFromStream requires a bitmapfileheader
    hdr.bfSize := data.size;    // before the data...
    hdr.bfReserved1 := 0;
    hdr.bfReserved2 := 0;
    hdr.bfOffBits := sizeof (hdr);

    s.Write (hdr, sizeof (hdr));
    data.Seek (0, soFromBeginning);
    s.CopyFrom (data, data.size);

    InternalGetImage (s, picture)
  finally
    s.Free
  end
end;

(*----------------------------------------------------------------------*
 | TBitmapResourceDetails.GetPixelFormat                                |
 *----------------------------------------------------------------------*)
function TBitmapResourceDetails.GetPixelFormat: TPixelFormat;
begin
  result := GetBitmapInfoPixelFormat (PBitmapInfoHeader (data.Memory)^);
end;

(*----------------------------------------------------------------------*
 | TBitmapResourceDetails.GetWidth                                      |
 *----------------------------------------------------------------------*)
function TBitmapResourceDetails.GetWidth: Integer;
begin
  result := PBitmapInfoHeader (data.Memory)^.biWidth
end;

(*----------------------------------------------------------------------*
 | TBitmapResourceDetails.SetImage                                      |
 *----------------------------------------------------------------------*)
procedure TBitmapResourceDetails.InitNew;
var
  bi : TBitmapInfoHeader;
  imageSize : DWORD;
  bits : PChar;
begin
  bi.biSize := SizeOf (bi);
  bi.biWidth := DefaultBitmapWidth;
  bi.biHeight := DefaultBitmapHeight;
  bi.biPlanes := 1;
  bi.biBitCount := GetPixelFormatBitCount (DefaultBitmapPixelFormat);
  bi.biCompression := BI_RGB;

  imageSize := BytesPerScanLine (DefaultBitmapWidth, bi.biBitCount, 32) * DefaultBitmapHeight;
  bi.biSizeImage := imageSize;

  bi.biXPelsPerMeter := 0;
  bi.biYPelsPerMeter := 0;

  bi.biClrUsed := 0;
  bi.biClrImportant := 0;

  data.Write (bi, SizeOf (bi));

  bits := AllocMem (ImageSize);
  try
    data.Write (bits^, ImageSize);
  finally
    ReallocMem (bits, 0)
  end
end;

procedure TBitmapResourceDetails.InternalGetImage(s : TStream; picture: TPicture);
var
  pHdr : PBitmapInfoHeader;
  pal : HPalette;
  colors : DWORD;
  hangOnToPalette : Boolean;
  newBmp : TBitmap;
begin
  s.Seek (0, soFromBeginning);
  picture.Bitmap.IgnorePalette := False;
  picture.Bitmap.LoadFromStream (s);

  pHdr := PBitmapInfoHeader (data.Memory);

                              // TBitmap makes all RLE encoded bitmaps into pfDevice
                              // ... that's not good enough for us!  At least
                              // select the correct pixel format, preserve their carefully set
                              // up palette, etc.
                              //
                              // But revisit this - we probably shouldn't call LoadFromStream
                              // at all if this is the case...
                              //
                              // You can get a couple of RLE bitmaps out of winhlp32.exe

  if PHdr^.biCompression in [BI_RLE4, BI_RLE8] then
  begin
    hangOnToPalette := False;
    if pHdr^.biBitCount in [1, 4, 8] then
    begin
      pal := picture.Bitmap.Palette;
      if pal <> 0 then
      begin
        colors := 0;
        GetObject (pal, SizeOf (colors), @Colors);

        if colors = 1 shl pHdr^.biBitCount then
        begin
          hangOnToPalette := True;

          newBmp := TBitmap.Create;
          try
            case pHdr^.biBitCount of
              1 : newBmp.PixelFormat := pf1Bit;
              4 : newBmp.PixelFormat := pf4Bit;
              8 : newBmp.PixelFormat := pf8Bit;
            end;

            newBmp.Width := Picture.Bitmap.Width;
            newBmp.Height := Picture.Bitmap.Height;
            newBmp.Palette := CopyPalette (pal);
            newBmp.Canvas.Draw (0, 0, picture.Bitmap);
            picture.Bitmap.Assign (newBmp);
          finally
            newBmp.Free
          end
        end
      end
    end;

    if not hangOnToPalette then
      case pHdr^.biBitCount of
        1 : picture.Bitmap.PixelFormat := pf1Bit;
        4 : picture.Bitmap.PixelFormat := pf4Bit;
        8 : picture.Bitmap.PixelFormat := pf8Bit;
        else
          picture.Bitmap.PixelFormat := pf24Bit
      end
  end
end;

(*----------------------------------------------------------------------*
 | TBitmapResourceDetails.InternalSetImage                              |
 |                                                                      |
 | Save image 'image' to stream 's' as a bitmap                         |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   s : TStream           The stream to save to                        |
 |   image : TPicture      The image to save                            |
 *----------------------------------------------------------------------*)
procedure TBitmapResourceDetails.InternalSetImage(s: TStream; image: TPicture);
var
  bmp : TBitmap;
begin
  s.Size := 0;
  bmp := TBitmap.Create;
  try
    bmp.Assign (image.graphic);
    bmp.SaveToStream (s);
  finally
    bmp.Free;
  end
end;

(*----------------------------------------------------------------------*
 | TBitmapResourceDetails.SetImage                                      |
 *----------------------------------------------------------------------*)
procedure TBitmapResourceDetails.SetImage(image : TPicture);
var
  s : TMemoryStream;
begin
  s := TMemoryStream.Create;
  try
    InternalSetImage (s, image);
    data.Clear;
    data.Write ((PChar (s.Memory) + sizeof (TBitmapFileHeader))^, s.Size - sizeof (TBitmapFileHeader));
  finally
    s.Free;
  end
end;

{ TIconGroupResourceDetails }

(*----------------------------------------------------------------------*
 | TIconGroupResourceDetails.GetBaseType                                |
 *----------------------------------------------------------------------*)
class function TIconGroupResourceDetails.GetBaseType: string;
begin
  result := IntToStr (Integer (RT_GROUP_ICON));
end;

{ TCursorGroupResourceDetails }

(*----------------------------------------------------------------------*
 | TCursorGroupResourceDetails.GetBaseType                              |
 *----------------------------------------------------------------------*)
class function TCursorGroupResourceDetails.GetBaseType: string;
begin
  result := IntToStr (Integer (RT_GROUP_CURSOR));
end;

{ TIconResourceDetails }

(*----------------------------------------------------------------------*
 | TIconResourceDetails.GetBaseType                                     |
 *----------------------------------------------------------------------*)
class function TIconResourceDetails.GetBaseType: string;
begin
  result := IntToStr (Integer (RT_ICON));
end;

{ TCursorResourceDetails }

(*----------------------------------------------------------------------*
 | TCursorResourceDetails.GetBaseType                                   |
 *----------------------------------------------------------------------*)
class function TCursorResourceDetails.GetBaseType: string;
begin
  result := IntToStr (Integer (RT_CURSOR));
end;

{ TGraphicsResourceDetails }


{ TIconCursorResourceDetails }

(*----------------------------------------------------------------------*
 | TIconCursorResourceDetails.GetHeight                                 |
 *----------------------------------------------------------------------*)
function TIconCursorResourceDetails.GetHeight: Integer;
var
  infoHeader : PBitmapInfoHeader;
begin
  if self is TCursorResourceDetails then        // Not very 'OOP'.  Sorry
    infoHeader := PBitmapInfoHeader (PChar (data.Memory) + sizeof (DWORD))
  else
    infoHeader := PBitmapInfoHeader (PChar (data.Memory));

  result := infoHeader.biHeight div 2
end;

(*----------------------------------------------------------------------*
 | TIconCursorResourceDetails.GetImage                                  |
 *----------------------------------------------------------------------*)
procedure TIconCursorResourceDetails.GetImage(picture: TPicture);
var
  iconCursor : TExIconCursor;
  strm : TMemoryStream;
  hdr : TIconHeader;
  dirEntry : TIconDirEntry;
  infoHeader : PBitmapInfoHeader;
begin
  if data.Size = 0 then Exit;


  strm := Nil;
  if self is TCursorResourceDetails then
  begin
    hdr.wType := 2;
    infoHeader := PBitmapInfoHeader (PChar (data.Memory) + sizeof (DWORD));
    iconCursor := TExCursor.Create
  end
  else
  begin
    hdr.wType := 1;
    infoHeader := PBitmapInfoHeader (PChar (data.Memory));
    iconCursor := TExIcon.Create
  end;

  try
    strm := TMemoryStream.Create;
    hdr.wReserved := 0;
    hdr.wCount := 1;

    strm.Write (hdr, sizeof (hdr));

    dirEntry.bWidth := infoHeader^.biWidth;
    dirEntry.bHeight := infoHeader^.biHeight div 2;
    dirEntry.bColorCount := GetBitmapInfoNumColors (infoHeader^);
    dirEntry.bReserved := 0;

    dirEntry.wPlanes := infoHeader^.biPlanes;
    dirEntry.wBitCount := infoHeader^.biBitCount;

    dirEntry.dwBytesInRes := data.Size;
    dirEntry.dwImageOffset := sizeof (hdr) + sizeof (dirEntry);

    strm.Write (dirEntry, sizeof (dirEntry));
    strm.CopyFrom (data, 0);
    strm.Seek (0, soFromBeginning);

    iconcursor.LoadFromStream (strm);
    picture.Graphic := iconcursor
  finally
    strm.Free;
    iconcursor.Free
  end
end;

(*----------------------------------------------------------------------*
 | TIconCursorResourceDetails.SetImage                                  |
 *----------------------------------------------------------------------*)
procedure TIconCursorResourceDetails.SetImage(image: TPicture);
var
  icon : TExIconCursor;
begin
  icon := TExIconCursor (image.graphic);
  data.Clear;
  data.CopyFrom (icon.Images [icon.CurrentImage].MemoryImage, 0);
end;


(*----------------------------------------------------------------------*
 | TIconCursorResourceDetails.GetPixelFormat                            |
 *----------------------------------------------------------------------*)
function TIconCursorResourceDetails.GetPixelFormat: TPixelFormat;
var
  infoHeader : PBitmapInfoHeader;
begin
  if self is TCursorResourceDetails then
    infoHeader := PBitmapInfoHeader (PChar (data.Memory) + sizeof (DWORD))
  else
    infoHeader := PBitmapInfoHeader (PChar (data.Memory));

  result := GetBitmapInfoPixelFormat (infoHeader^);
end;

(*----------------------------------------------------------------------*
 | TIconCursorResourceDetails.GetWidth                                  |
 *----------------------------------------------------------------------*)
function TIconCursorResourceDetails.GetWidth: Integer;
var
  infoHeader : PBitmapInfoHeader;
begin
  if self is TCursorResourceDetails then
    infoHeader := PBitmapInfoHeader (PChar (data.Memory) + sizeof (DWORD))
  else
    infoHeader := PBitmapInfoHeader (PChar (data.Memory));

  result := infoHeader.biWidth
end;

{ TIconCursorGroupResourceDetails }

(*----------------------------------------------------------------------*
 | TIconCursorGroupResourceDetails.BeforeDelete
 |                                                                      |
 *----------------------------------------------------------------------*)
procedure TIconCursorGroupResourceDetails.AddToGroup(
  details: TIconCursorResourceDetails);
var
  attributes : PResourceDirectory;
  infoHeader : PBitmapInfoHeader;
  cc : Integer;
begin
  data.Size := Data.Size + sizeof (TResourceDirectory);
  attributes := PResourceDirectory (PChar (Data.Memory) + sizeof (TIconHeader));

  Inc (Attributes, PIconHeader (data.Memory)^.wCount);

  attributes^.wNameOrdinal :=  StrToInt (details.ResourceName);
  attributes^.lBytesInRes := details.Data.Size;

  if details is TIconResourceDetails then
  begin
    infoHeader := PBitmapInfoHeader (PChar (details.data.Memory));
    attributes^.details.iconWidth := infoHeader^.biWidth;
    attributes^.details.iconHeight := infoHeader^.biHeight div 2;
    cc := GetBitmapInfoNumColors (infoHeader^);
    if cc < 256 then
      attributes^.details.iconColorCount := cc
    else
      attributes^.details.iconColorCount := 0;
    attributes^.details.iconReserved := 0
  end
  else
  begin
    infoHeader := PBitmapInfoHeader (PChar (details.data.Memory) + sizeof (DWORD));
    attributes^.details.cursorWidth := infoHeader^.biWidth;
    attributes^.details.cursorHeight := infoHeader^.biHeight div 2
  end;

  attributes^.wPlanes := infoHeader^.biPlanes;
  attributes^.wBitCount := infoHeader^.biBitCount;

  Inc (PIconHeader (data.Memory)^.wCount);
end;

procedure TIconCursorGroupResourceDetails.BeforeDelete;
begin
  fDeleting := True;
  try
    while ResourceCount > 0 do
      Parent.DeleteResource (Parent.IndexOfResource (ResourceDetails [0]));
  finally
    fDeleting := False
  end
end;

(*----------------------------------------------------------------------*
 | TIconCursorGroupResourceDetails.Contains                             |
 *----------------------------------------------------------------------*)
function TIconCursorGroupResourceDetails.Contains(
  details: TIconCursorResourceDetails): Boolean;
var
  i, id : Integer;
  attributes : PResourceDirectory;
begin
  Result := False;
  if ResourceNameToInt (details.ResourceType) = ResourceNameToInt (ResourceType) - DIFFERENCE then
  begin
    attributes := PResourceDirectory (PChar (Data.Memory) + sizeof (TIconHeader));
    id := ResourceNameToInt (details.ResourceName);

    for i := 0 to PIconHeader (Data.Memory)^.wCount - 1 do
      if attributes^.wNameOrdinal = id then
      begin
        Result := True;
        break
      end
      else
        Inc (attributes)
  end
end;

(*----------------------------------------------------------------------*
 | TIconCursorGroupResourceDetails.GetImage                             |
 *----------------------------------------------------------------------*)
procedure TIconCursorGroupResourceDetails.GetImage(picture: TPicture);
var
  i, hdrOffset, imgOffset : Integer;
  iconCursor : TExIconCursor;
  strm : TMemoryStream;
  hdr : TIconHeader;
  dirEntry : TIconDirEntry;
  pdirEntry : PIconDirEntry;
  infoHeader : PBitmapInfoHeader;
begin
  if data.Size = 0 then Exit;

  strm := Nil;
  if self is TCursorGroupResourceDetails then
  begin
    hdr.wType := 2;
    hdrOffset := SizeOf (DWORD);
    iconCursor := TExCursor.Create
  end
  else
  begin
    hdr.wType := 1;
    hdrOffset := 0;
    iconCursor := TExIcon.Create
  end;

  try
    strm := TMemoryStream.Create;
    hdr.wReserved := 0;
    hdr.wCount := ResourceCount;

    strm.Write (hdr, sizeof (hdr));

    for i := 0 to ResourceCount - 1 do
    begin
      infoHeader := PBitmapInfoHeader (PChar (ResourceDetails [i].Data.Memory) + hdrOffset);
      dirEntry.bWidth := infoHeader^.biWidth;
      dirEntry.bHeight := infoHeader^.biHeight div 2;
      dirEntry.wPlanes := infoHeader^.biPlanes;
      dirEntry.bColorCount := GetBitmapInfoNumColors (infoHeader^);
      dirEntry.bReserved := 0;
      dirEntry.wBitCount := infoHeader^.biBitCount;
      dirEntry.dwBytesInRes := resourceDetails [i].data.Size;
      dirEntry.dwImageOffset := 0;

      strm.Write (dirEntry, sizeof (dirEntry));
    end;

    for i := 0 to ResourceCount - 1 do
    begin
      imgOffset := strm.Position;
      pDirEntry := PIconDirEntry (PChar (strm.Memory) + SizeOf (TIconHeader) + i * SizeOf (TIconDirEntry));
      pDirEntry^.dwImageOffset := imgOffset;

      strm.CopyFrom (ResourceDetails [i].Data, 0);
    end;

    if ResourceCount > 0 then
    begin
      strm.Seek (0, soFromBeginning);
      iconcursor.LoadFromStream (strm);
      picture.Graphic := iconcursor
    end
    else
      picture.Graphic := Nil
  finally
    strm.Free;
    iconcursor.Free
  end
end;

(*----------------------------------------------------------------------*
 | TIconCursorGroupResourceDetails.GetResourceCount                     |
 *----------------------------------------------------------------------*)
function TIconCursorGroupResourceDetails.GetResourceCount: Integer;
begin
  result := PIconHeader (Data.Memory)^.wCount
end;

(*----------------------------------------------------------------------*
 | TIconCursorGroupResourceDetails.GetResourceDetails                   |
 *----------------------------------------------------------------------*)
function TIconCursorGroupResourceDetails.GetResourceDetails(
  idx: Integer): TIconCursorResourceDetails;
var
  i : Integer;
  res : TResourceDetails;
  attributes : PResourceDirectory;
  iconCursorResourceType : string;
begin
  result := Nil;
  attributes := PResourceDirectory (PChar (Data.Memory) + sizeof (TIconHeader));
  Inc (attributes, idx);

  // DIFFERENCE (from Windows.pas) is 11.  It's the difference between a 'group
  // resource' and the resource itself.  They called it 'DIFFERENCE' to be annoying.

  iconCursorResourceType := IntToStr (ResourceNameToInt (ResourceType) - DIFFERENCE);
  for i := 0 to Parent.ResourceCount - 1 do
  begin
    res := Parent.ResourceDetails [i];
    if (res is TIconCursorResourceDetails) and (iconCursorResourceType = res.ResourceType) and (attributes.wNameOrdinal = ResourceNameToInt (res.ResourceName)) then
    begin
      result := TIconCursorResourceDetails (res);
      break
    end
  end
end;

(*----------------------------------------------------------------------*
 | TIconCursorGroupResourceDetails.InitNew                              |
 *----------------------------------------------------------------------*)
procedure TIconCursorGroupResourceDetails.InitNew;
var
  imageResource : TIconCursorResourceDetails;
  iconHeader : TIconHeader;
  dir : TResourceDirectory;
  nm : string;

begin
  iconHeader.wCount := 1;
  iconHeader.wReserved := 0;

  if Self is TCursorGroupResourceDetails then
  begin
    iconHeader.wType := 2;
    nm := Parent.GetUniqueResourceName (TCursorResourceDetails.GetBaseType);
    imageResource := TCursorResourceDetails.CreateNew (Parent, ResourceLanguage, nm)
  end
  else
  begin
    iconHeader.wType := 1;
    nm := Parent.GetUniqueResourceName (TIconResourceDetails.GetBaseType);
    imageResource := TIconResourceDetails.CreateNew (Parent, ResourceLanguage, nm)
  end;

  data.Write (iconHeader, SizeOf (iconHeader));

  if Self is TIconGroupResourceDetails then
  begin
    dir.details.iconWidth := DefaultIconCursorWidth;
    dir.details.iconHeight := DefaultIconCursorHeight;
    dir.details.iconColorCount := GetPixelFormatNumColors (DefaultIconCursorPixelFormat);
    dir.details.iconReserved := 0
  end
  else
  begin
    dir.details.cursorWidth := DefaultIconCursorWidth;
    dir.details.cursorHeight := DefaultIconCursorHeight
  end;

  dir.wPlanes := 1;
  dir.wBitCount := GetPixelFormatBitCount (DefaultIconCursorPixelFormat);
  dir.lBytesInRes := imageResource.Data.Size;
  dir.wNameOrdinal := ResourceNametoInt (imageResource.ResourceName);

  data.Write (dir, SizeOf (dir));
end;

(*----------------------------------------------------------------------*
 | TIconCursorResourceDetails.BeforeDelete                              |
 |                                                                      |
 | If we're deleting an icon/curor resource, remove its reference from  |
 | the icon/cursor group resource.                                      |
 *----------------------------------------------------------------------*)
procedure TIconCursorResourceDetails.BeforeDelete;
var
  i : Integer;
  details : TResourceDetails;
  resGroup : TIconCursorGroupResourceDetails;
begin
  for i := 0 to Parent.ResourceCount - 1 do
  begin
    details := Parent.ResourceDetails [i];
    if (details.ResourceType = IntToStr (ResourceNameToInt (ResourceType) + DIFFERENCE)) then
    begin
      resGroup := details as TIconCursorGroupResourceDetails;
      if resGroup.Contains (Self) then
      begin
        resGroup.RemoveFromGroup (Self);
        break
      end
    end
  end
end;

procedure TIconCursorGroupResourceDetails.LoadImage(
  const FileName: string);
var
  img : TExIconCursor;
  hdr : TIconHeader;
  i : Integer;
  dirEntry : TResourceDirectory;
  res : TIconCursorResourceDetails;
  resTp : string;
begin
  BeforeDelete;         // Make source there are no existing image resources

  if Self is TIconGroupResourceDetails then
  begin
    hdr.wType := 1;
    img := TExIcon.Create;
    resTp := TIconResourceDetails.GetBaseType;
  end
  else
  begin
    hdr.wType := 2;
    img := TExCursor.Create;
    resTp := TCursorResourceDetails.GetBaseType;
  end;

  img.LoadFromFile (FileName);

  hdr.wReserved := 0;
  hdr.wCount := img.ImageCount;

  data.Clear;

  data.Write (hdr, SizeOf (hdr));

  for i := 0 to img.ImageCount - 1 do
  begin
    if hdr.wType = 1 then
    begin
      dirEntry.details.iconWidth := img.Images [i].FWidth;
      dirEntry.details.iconHeight := img.Images [i].FHeight;
      dirEntry.details.iconColorCount := GetPixelFormatNumColors (img.Images [i].FPixelFormat);
      dirEntry.details.iconReserved := 0
    end
    else
    begin
      dirEntry.details.cursorWidth := img.Images [i].FWidth;
      dirEntry.details.cursorHeight := img.Images [i].FHeight;
    end;

    dirEntry.wPlanes := 1;
    dirEntry.wBitCount := GetPixelFormatBitCount (img.Images [i].FPixelFormat);

    dirEntry.lBytesInRes := img.Images [i].FMemoryImage.Size;

    if hdr.wType = 1 then
      res := TIconResourceDetails.Create (Parent, ResourceLanguage, Parent.GetUniqueResourceName (resTp), resTp, img.Images [i].FMemoryImage.Size, img.Images [i].FMemoryImage.Memory)
    else
      res := TCursorResourceDetails.Create (Parent, ResourceLanguage, Parent.GetUniqueResourceName (resTp), resTp, img.Images [i].FMemoryImage.Size, img.Images [i].FMemoryImage.Memory);
    Parent.AddResource (res);
    dirEntry.wNameOrdinal := ResourceNameToInt (res.ResourceName);

    data.Write (dirEntry, SizeOf (dirEntry));
  end
end;

(*----------------------------------------------------------------------*
 | TIconCursorGroupResourceDetails.RemoveFromGroup                      |
 *----------------------------------------------------------------------*)
procedure TIconCursorGroupResourceDetails.RemoveFromGroup(
  details: TIconCursorResourceDetails);
var
  i, id, count : Integer;
  attributes, ap : PResourceDirectory;
begin
  if ResourceNametoInt (details.ResourceType) = ResourceNameToInt (ResourceType) - DIFFERENCE then
  begin
    attributes := PResourceDirectory (PChar (Data.Memory) + sizeof (TIconHeader));
    id := ResourceNametoInt (details.ResourceName);

    Count := PIconHeader (Data.Memory)^.wCount;

    for i := 0 to Count - 1 do
      if attributes^.wNameOrdinal = id then
      begin
        if i < Count - 1 then
        begin
          ap := Attributes;
          Inc (ap);
          Move (ap^, Attributes^, SizeOf (TResourceDirectory) * (Count - i - 1));
        end;

        Data.Size := data.Size - SizeOf (TResourceDirectory);
        PIconHeader (Data.Memory)^.wCount := Count - 1;
        if (Count = 1) and not fDeleting then
          Parent.DeleteResource (Parent.IndexOfResource (Self));
        break
      end
      else
        Inc (attributes)
  end
end;

(*----------------------------------------------------------------------*
 | TIconCursorResourceDetails.InitNew                                   |
 *----------------------------------------------------------------------*)
procedure TIconCursorResourceDetails.InitNew;
var
  hdr : TBitmapInfoHeader;
  cImageSize : DWORD;
  pal : HPALETTE;
  entries : PPALETTEENTRY;
  w : DWORD;
  p : PChar;

begin
  if Self is TCursorResourceDetails then
    Data.Write (DefaultCursorHotspot, SizeOf (DefaultCursorHotspot));

  hdr.biSize := SizeOf (hdr);
  hdr.biWidth := DefaultIconCursorWidth;
  hdr.biHeight := DefaultIconCursorHeight * 2;
  hdr.biPlanes := 1;
  hdr.biBitCount := GetPixelFormatBitCount (DefaultIconCursorPixelFormat);

  if DefaultIconCursorPixelFormat = pf16Bit then
    hdr.biCompression := BI_BITFIELDS
  else
    hdr.biCompression := BI_RGB;

  hdr.biSizeImage := 0; // See note in unitExIcon

  hdr.biXPelsPerMeter := 0;
  hdr.biYPelsPerMeter := 0;

  hdr.biClrUsed := GetPixelFormatNumColors (DefaultIconCursorPixelFormat);
  hdr.biClrImportant := hdr.biClrUsed;

  Data.Write (hdr, SizeOf (hdr));

  pal := 0;
  case DefaultIconCursorPixelFormat of
    pf1Bit : pal := SystemPalette2;
    pf4Bit : pal := SystemPalette16;
    pf8Bit : pal := SystemPalette256
  end;

  entries := Nil;
  try
    if pal > 0 then
    begin
      GetMem (entries, hdr.biClrUsed * sizeof (PALETTEENTRY));
      GetPaletteEntries (pal, 0, hdr.biClrUsed, entries^);

      data.Write (entries^, hdr.biClrUsed * SizeOf (PALETTEENTRY))
    end
    else
      if hdr.biCompression = BI_BITFIELDS then
      begin { 5,6,5 bitfield }
        w := $0f800;  // 1111 1000 0000 0000  5 bit R mask
        data.Write (w, SizeOf (w));
        w := $07e0;   // 0000 0111 1110 0000  6 bit G mask
        data.Write (w, SizeOf (w));
        w := $001f;   // 0000 0000 0001 1111  5 bit B mask
        data.Write (w, SizeOf (w))
      end

  finally
    ReallocMem (entries, 0)
  end;

  // Write dummy image
  cImageSize := BytesPerScanLine (hdr.biWidth, hdr.biBitCount, 32) * DefaultIconCursorHeight;
  p := AllocMem (cImageSize);
  try
    data.Write (p^, cImageSize);
  finally
    ReallocMem (p, 0)
  end;

  // Write dummy mask
  cImageSize := DefaultIconCursorHeight * DefaultIconCursorWidth div 8;

  GetMem (p, cImageSize);
  FillChar (p^, cImageSize, $ff);

  try
    data.Write (p^, cImageSize);
  finally
    ReallocMem (p, 0)
  end;
end;

{ TDIBResourceDetails }

class function TDIBResourceDetails.GetBaseType: string;
begin
  Result := 'DIB';
end;

procedure TDIBResourceDetails.GetImage(picture: TPicture);
begin
  InternalGetImage (data, Picture);
end;

procedure TDIBResourceDetails.InitNew;
var
  hdr : TBitmapFileHeader;
begin
  hdr.bfType := $4d42;
  hdr.bfSize := SizeOf (TBitmapFileHeader) + SizeOf (TBitmapInfoHeader);
  hdr.bfReserved1 := 0;
  hdr.bfReserved2 := 0;
  hdr.bfOffBits := hdr.bfSize;
  data.Write (hdr, SizeOf (hdr));

  inherited;
end;

procedure TDIBResourceDetails.SetImage(image: TPicture);
begin
  InternalSetImage (data, image);
end;

class function TDIBResourceDetails.SupportsData(Size: Integer;
  data: Pointer): Boolean;
var
  p : PBitmapFileHeader;
  hdrSize : DWORD;
begin
  Result := False;
  p := PBitmapFileHeader (data);
  if (p^.bfType = $4d42) and (p^.bfReserved1 = 0) and (p^.bfReserved2 = 0) then
  begin
    hdrSize := PDWORD (PChar (data) + SizeOf (TBitmapFileHeader))^;

    case hdrSize of
      SizeOf (TBitmapInfoHeader) : Result := True;
      SizeOf (TBitmapV4Header) : Result := True;
      SizeOf (TBitmapV5Header) : Result := True
    end
  end
end;

{ TGraphicsResourceDetails }

procedure TGraphicsResourceDetails.SetImage(image: TPicture);
begin
  data.Clear;
  image.Graphic.SaveToStream (data);
end;

initialization
  TPicture.RegisterFileFormat ('ICO', rstIcons, TExIcon);
  TPicture.RegisterFileFormat ('CUR', rstCursors, TExCursor);
  TPicture.UnregisterGraphicClass (TIcon);


  RegisterResourceDetails (TBitmapResourceDetails);
  RegisterResourceDetails (TDIBResourceDetails);
  RegisterResourceDetails (TIconGroupResourceDetails);
  RegisterResourceDetails (TCursorGroupResourceDetails);
  RegisterResourceDetails (TIconResourceDetails);
  RegisterResourceDetails (TCursorResourceDetails);
finalization
  TPicture.UnregisterGraphicClass (TExIcon);
  TPicture.UnregisterGraphicClass (TExCursor);
  TPicture.RegisterFileFormat ('ICO', 'Icon', TIcon);
  UnregisterResourceDetails (TCursorResourceDetails);
  UnregisterResourceDetails (TIconResourceDetails);
  UnregisterResourceDetails (TCursorGroupResourceDetails);
  UnregisterResourceDetails (TIconGroupResourceDetails);
  UnregisterResourceDetails (TDIBResourceDetails);
  UnregisterResourceDetails (TBitmapResourceDetails);
end.
