{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImageList.pas, released on 2003-10-09

The Initial Developers of the Original Code are: Andreas Hausladen <Andreas dott Hausladen att gmx dott de>
Copyright (c) 2003 Andreas Hausladen
All Rights Reserved.
Portions created by Uwe Schuster are Copyright (C) 2003, 2004 Uwe Schuster.

Contributor(s):
Uwe Schuster [jedivcs att bitcommander dott de]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  ImageKind ikMappedResourceBitmap is not support so far
-----------------------------------------------------------------------------}
// $Id$

unit JvImageList;

{$I jvcl.inc}

interface

uses
  Windows,
  {$IFDEF MSWINDOWS}
  CommCtrl,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes, Graphics, Controls, ImgList;

type
  TJvImageListMode = (imClassic, imPicture, imResourceIds, imItemList);
  TJvImageListTransparentMode = (tmNone, tmAuto, tmColor);

  EJvImageListError = class(Exception);

  TJvImageListItemKind = (ikResourceBitmap, ikMappedResourceBitmap, ikInlineBitmap);

  TJvImageListItem = class(TCollectionItem)
  private
    FBitmap: TBitmap;
    FKind: TJvImageListItemKind;
    FResourceName: string;
    FTransparentColor: TColor;
    procedure AddToImageList(AImageList: TImageList);
    procedure BitmapChanged(Sender: TObject);
    function GetImageList: TImageList;
    procedure SetBitmap(ABitmap: TBitmap);
    procedure SetKind(AKind: TJvImageListItemKind);
    procedure SetResourceName(const AResourceName: string);
    procedure SetTransparentColor(AColor: TColor);
    procedure UpdateImageListItem(AImageList: TImageList; AIndex: Integer);
  protected
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure UpdateImageList;
  published
    property Kind: TJvImageListItemKind read FKind write SetKind;
    property TransparentColor: TColor read FTransparentColor write
      SetTransparentColor;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property ResourceName: string read FResourceName write SetResourceName;
  end;

  TJvImageListItems = class(TOwnedCollection)
  private
    function GetItem(AIndex: Integer): TJvImageListItem;
    procedure SetItem(AIndex: Integer; Value: TJvImageListItem);
  protected
    {$IFDEF COMPILER5}
    function Owner: TPersistent;
    {$ENDIF COMPILER5}
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent);
    function Add: TJvImageListItem;
    property Items[AIndex: Integer]: TJvImageListItem read GetItem write SetItem; default;
  end;

  TJvImageList = class(TImageList)
  private
    FUpdateLock: Integer;
    FModified: Boolean;

    FItems: TJvImageListItems;
    FTransparentMode: TJvImageListTransparentMode;
    FTransparentColor: TColor;
    FPicture: TPicture;
    FFileName: TFileName;
    {$IFDEF VCL}
    FPixelFormat: TPixelFormat;
    {$ENDIF VCL}
    FResourceIds: TStrings;
    FMode: TJvImageListMode;

    procedure SetFileName(const Value: TFileName);
    procedure SetItems(AItems: TJvImageListItems);
    procedure SetPicture(Value: TPicture);
    procedure SetTransparentMode(Value: TJvImageListTransparentMode);
    procedure SetTransparentColor(Value: TColor);
    {$IFDEF VCL}
    procedure SetPixelFormat(const Value: TPixelFormat);
    procedure SetInternalHandle(Value: THandle);
    {$ENDIF VCL}
    procedure SetResourceIds(Value: TStrings);
    procedure SetMode(const Value: TJvImageListMode);

    procedure SlicePictureToImageList;
    procedure ResourceIdsToImageList;
    procedure DoLoadFromFile;
  protected
    procedure ItemListError;
    procedure DefineProperties(Filer: TFiler); override;
    procedure InitializeImageList; virtual; // called by Initialize (VCL and VCLX)
    {$IFDEF VCL}
    procedure Initialize; override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure Initialize(const AWidth, AHeight: Integer); override;
    {$ENDIF VisualCLX}
    procedure Change; override;
    procedure DataChanged(Sender: TObject); virtual;
    procedure UpdateImageList;
    {$IFDEF VCL}
    procedure HandleNeeded; virtual;
    procedure CreateImageList; virtual;
    property FHandle: THandle write SetInternalHandle;
    {$ENDIF VCL}
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    {$IFDEF VCL}
    procedure DrawIndirect(ImageListDrawParams: TImageListDrawParams);
      // DrawIndirect fills the .cbSize and .himl field.
    function Merge(Index1: Integer; ImageList: TImageList; Index2: Integer;
      dx, dy: Integer): TImageList;
      // Merge creates a new TJvImageList and returns it. It is up to the user
      // to release this new image list.
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure GetIcon(Index: Integer; Ico: TIcon);
    {$ENDIF VisualCLX}
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream); virtual;

    { imItemList }
    // (usc) DeleteItem, ClearItem and GetItemInfoStr are obsolete because the are
    //       directly mapped to Items
    procedure AddItem(ABitmap: TBitmap; ATransparentColor: TColor); overload;
      // AddItem adds a bitmap to the ItemList with ATransparentColor as
      // transparent color. If the image list mode is not imItemList the image
      // list is cleared and the mode is set to imItemList.
    procedure AddItem(const AResourceName: string; ATransparentColor: TColor); overload;
      // AddItem adds the resource AResourceName from the HInstance libarary to
      // the ItemList with ATransparentColor as transparent color. If the image
      // list mode is not imItemList the image list is cleared and the mode is
      // set to imItemList.
    procedure DeleteItem(AIndex: Integer);
      // DeleteItem deletes the ItemList item that is identified by AIndex.
      // When the ImageList is not in imItenList mode the method raises an
      // RJvImageListError.
    procedure ClearItems;
      // ClearItems clears the ItemList. When the ImageList is not in imItemList
      // mode the method raises an RJvImageListError.
    function GetItemInfoStr(AIndex: Integer): string;
      // GetItemInfoStr returns the info string of the ItemList item that is
      // identified by AIndex. When the ImageList is not in imItenList mode the
      // method raises an RJvImageListError.
  published
    property Mode: TJvImageListMode read FMode write SetMode default imPicture;
      // Mode specifies which property the component should use.
      //   imClassic: be a normal TImageList
      //   imPicture: split the image in Picture
      //   imResourceIds: load the images by ResourceIds
      //   imItemList: the AddItem, DeleteItem, ClearItems and GetItemInfoStr methods are available

    {$IFDEF VCL}
    property PixelFormat: TPixelFormat read FPixelFormat write SetPixelFormat default pfDevice;
      // PixelFormat is the color resolution of the image list. pf1bit and
      // pfCustom are not supported.
      // WARNING: pf32bit works under Windows XP only.
    {$ENDIF VCL}
    property TransparentMode: TJvImageListTransparentMode read FTransparentMode write SetTransparentMode default
      tmColor;
      // TransparentMode is used for adding the bitmaps from Picture or
      // ResourceIds.
      //   tmNone: no mask
      //   tmAuto: use the pixel at the left bottom edge
      //   tmColor: use TransparentColor
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor default clFuchsia;
      // TransparentColor specifies the color that is used as the MaskColor
      // when spitting the graphic in Picture.Graphic or adding the Resource
      // bitmaps to the image list.

    property FileName: TFileName read FFileName write SetFileName;
      // (only for designtime)
      // FileName specifies a graphic file that is available on the developer's
      // system which contains the bitmaps which can be exported by the
      // ImageList. The Data is copied to Picture.Graphic. If the file does not
      // exists at design time the stored Picture.Graphic is used.
    property Picture: TPicture read FPicture write SetPicture;
      // Picture.Graphic is updated at design time by the graphic file specified
      // by FileName. The Picture property is only loaded into the image list if
      // the Mode is imPicture.
    property ResourceIds: TStrings read FResourceIds write SetResourceIds;
      // ResourceIds contains the resource ids of the bitmaps to load. Allowed
      // are RCDATA (a bitmap file) and BITMAP. ResourceIds property is only
      // loaded into the image list if Mode is imResourceIds.
    property Items: TJvImageListItems read FItems write SetItems;
  end;

{$IFDEF VCL}
function CreateImageListHandle(Width, Height: Integer; PixelFormat: TPixelFormat;
  Masked: Boolean; AllocBy: Integer): THandle;
{$ENDIF VCL}

function LoadImageListFromBitmap(ImgList: TCustomImageList; const Bitmap: TBitmap;
  MaskColor: TColor = clFuchsia; AutoMaskColor: Boolean = False): Integer; overload;
function LoadImageListFromBitmap(ImgList: TCustomImageList; const Bitmap: TBitmap;
  MaskBitmap: TBitmap): Integer; overload;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Consts, TypInfo,
  {$IFDEF VCL}
  ActiveX,
  {$ENDIF VCL}
  {$IFDEF COMPILER5}
  JvJclUtils, // SameFileName() for Delphi 5
  {$ENDIF COMPILER5}
  JvJVCLUtils, JvResources;

resourcestring
  // (usc) there is no real need to move this string to JvResource.pas because
  //       hopefully ikMappedResourceBitmap will be supported soon
  RsNotSupportedItemKind = 'The item kind %s is not supported so far.';

{$IFDEF UNIX}
const
  RT_RCDATA = PChar(10);
{$ENDIF UNIX}

{$IFDEF VCL}

{------------------------------------------------------------------------------}
{ Here we inject a jump to our HandleNeededHook into the static
  TCustomImageList.HandleNeeded method. }

type
  TCustomImageListAccessProtected = class(TCustomImageList);

  // we need direct access to the FHandle field because the Handle property
  // calls the Changed method that calls HandleNeeded that calls SetHandle, ...
  TImageListPrivate = class(TComponent)
  protected
    FHeight: Integer;
    FWidth: Integer;
    FAllocBy: Integer;
    FHandle: HIMAGELIST;
  end;

  TJumpCode = packed record
    Jump: Byte;
    Offset: Integer;
  end;

var
  HandleNeededHookInstalled: Boolean = False;
  SavedNeededHookCode: TJumpCode;

procedure HandleNeededHook(Self: TImageList);
begin
  if Self is TJvImageList then
    TJvImageList(Self).HandleNeeded
  else
  begin
    if not Self.HandleAllocated then
    begin
      TImageListPrivate(Self).FHandle := CreateImageListHandle(Self.Width, Self.Height,
        pfCustom, Self.Masked, Self.AllocBy);
      if not Self.HandleAllocated then
        raise EInvalidOperation.CreateRes(@SInvalidImageList);
      if Self.BkColor <> clNone then
        Self.BkColor := Self.BkColor;
    end;
  end;
end;

procedure UninstallHandleNeededHook;
var
  OrgProc: Pointer;
  n: Cardinal;
begin
  if HandleNeededHookInstalled then
  begin
    OrgProc := @TCustomImageListAccessProtected.HandleNeeded;

    if WriteProcessMemory(GetCurrentProcess, OrgProc, @SavedNeededHookCode, SizeOf(SavedNeededHookCode), n) then
    begin
      HandleNeededHookInstalled := False;
      FlushInstructionCache(GetCurrentProcess, OrgProc, SizeOf(SavedNeededHookCode));
    end;
  end;
end;

procedure InstallHandleNeededHook;
var
  OrgProc: Pointer;
  NewProc: Pointer;
  Code: TJumpCode;
  n: Cardinal;
begin
  if not HandleNeededHookInstalled then
  begin
    OrgProc := @TCustomImageListAccessProtected.HandleNeeded;
    NewProc := @HandleNeededHook;

    Code.Jump := $E9;
    Code.Offset := Integer(NewProc) - Integer(OrgProc) - SizeOf(Code);

    if ReadProcessMemory(GetCurrentProcess, OrgProc, @SavedNeededHookCode, SizeOf(SavedNeededHookCode), n) then
      if WriteProcessMemory(GetCurrentProcess, OrgProc, @Code, SizeOf(Code), n) then
      begin
        HandleNeededHookInstalled := True;
        FlushInstructionCache(GetCurrentProcess, OrgProc, SizeOf(Code));
      end;
  end;
end;

{------------------------------------------------------------------------------}

function CreateImageListHandle(Width, Height: Integer; PixelFormat: TPixelFormat;
  Masked: Boolean; AllocBy: Integer): THandle;
var
  Flags: Cardinal;
begin
  if PixelFormat = pfDevice then
    PixelFormat := ScreenPixelFormat;

  case PixelFormat of
    pf4bit:
      Flags := ILC_COLOR4;
    pf8bit:
      Flags := ILC_COLOR8;
    pf15bit, pf16bit:
      Flags := ILC_COLOR16;
    pf24bit:
      Flags := ILC_COLOR24;
    pf32bit:
      Flags := ILC_COLOR32;
  else
    Flags := ILC_COLORDDB;
  end;
  if Masked then
    Flags := Flags or ILC_MASK;

  Result := ImageList_Create(Width, Height, Flags, AllocBy, AllocBy);
end;

{$ENDIF VCL}

//=== { TJvImageListItem } ===================================================

constructor TJvImageListItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FBitmap := TBitmap.Create;
  FBitmap.OnChange := BitmapChanged;
  FKind := ikResourceBitmap;
  FResourceName := '';
  FTransparentColor := clFuchsia;
  if GetImageList <> nil then
    AddToImageList(GetImageList);
end;

destructor TJvImageListItem.Destroy;
var
  ImageList: TImageList;
begin
  ImageList := GetImageList;
  if Assigned(ImageList) and (Index >= 0) and (ImageList.Count > Index) then
    ImageList.Delete(Index);
  FBitmap.Free;
  inherited Destroy;
end;

procedure TJvImageListItem.AddToImageList(AImageList: TImageList);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := AImageList.Width;
    Bitmap.Height := AImageList.Height;
    AImageList.AddMasked(Bitmap, FTransparentColor);
  finally
    Bitmap.Free;
  end;
  UpdateImageListItem(AImageList, Pred(AImageList.Count));
end;

procedure TJvImageListItem.BitmapChanged(Sender: TObject);
begin
  UpdateImageList;
end;

function TJvImageListItem.GetDisplayName: string;
begin
  case FKind of
    ikResourceBitmap:
      Result := Format(RsResource, [FResourceName]);
    ikMappedResourceBitmap:
      Result := Format(RsMappedResource, [FResourceName]);
    ikInlineBitmap:
      Result := Format(RsBitmap,
        [GetEnumName(TypeInfo(TPixelFormat), Ord(FBitmap.PixelFormat))]);
  else
    inherited GetDisplayName;
  end;
end;

function TJvImageListItem.GetImageList: TImageList;
begin
  Result := TImageList(TJvImageListItems(Collection).Owner);
end;

procedure TJvImageListItem.SetBitmap(ABitmap: TBitmap);
begin
  if FKind = ikInlineBitmap then
  begin
    FBitmap.Assign(ABitmap);
    UpdateImageList;
  end;
end;

procedure TJvImageListItem.SetIndex(Value: Integer);
var
  ImageList: TImageList;
  OldIndex: Integer;
begin
  OldIndex := Index;
  inherited SetIndex(Value);
  ImageList := GetImageList;
  if Assigned(ImageList) and (OldIndex >= 0) and (ImageList.Count > OldIndex) and
    (Index >= 0) and (ImageList.Count > Index) then
    ImageList.Move(OldIndex, Index);
end;

procedure TJvImageListItem.SetKind(AKind: TJvImageListItemKind);
begin
  // (usc) remove when MappedResourceBitmap support is finished
  if AKind = ikMappedResourceBitmap then
    raise EJvImageListError.CreateResFmt(@RsNotSupportedItemKind, ['ikMappedResourceBitmap']);

  if FKind <> AKind then
  begin
    FKind := AKind;
    if FKind in [ikResourceBitmap, ikMappedResourceBitmap] then
      FBitmap.Assign(nil)
    else
    if FKind = ikInlineBitmap then
      FResourceName := '';
  end;
end;

procedure TJvImageListItem.SetResourceName(const AResourceName: string);
begin
  if (FKind in [ikResourceBitmap, ikMappedResourceBitmap]) and
    (FResourceName <> AResourceName) then
  begin
    FResourceName := AResourceName;
    UpdateImageList;
  end;
end;

procedure TJvImageListItem.SetTransparentColor(AColor: TColor);
begin
  if FTransparentColor <> AColor then
  begin
    FTransparentColor := AColor;
    UpdateImageList;
  end;
end;

procedure TJvImageListItem.UpdateImageList;
begin
  UpdateImageListItem(GetImageList, Index);
end;

procedure TJvImageListItem.UpdateImageListItem(AImageList: TImageList; AIndex: Integer);
var
  Bitmap: TBitmap;
begin
  if (FKind in [ikResourceBitmap, ikMappedResourceBitmap]) and (FResourceName <> '') then
  begin
    Bitmap := TBitmap.Create;
    try
      try
        if FKind = ikResourceBitmap then
          Bitmap.LoadFromResourceName(HInstance, FResourceName);
{// (usc) include when MappedResourceBitmap support is finished
        else
        if FKind = ikMappedResourceBitmap then
          GetMappedResourceBitmap(FResourceName, Bitmap);
}
        AImageList.ReplaceMasked(AIndex, Bitmap, FTransparentColor);
      except
      end;
    finally
      Bitmap.Free;
    end;
  end
  else
  if (FKind = ikInlineBitmap) and Assigned(FBitmap) and
    (FBitmap.Width = AImageList.Width) and (FBitmap.Height = AImageList.Height) then
    AImageList.ReplaceMasked(AIndex, FBitmap, FTransparentColor);
end;

//=== { TJvImageListItems } ==================================================

constructor TJvImageListItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TJvImageListItem);
end;

function TJvImageListItems.Add: TJvImageListItem;
begin
  Result := TJvImageListItem(inherited Add);
end;

function TJvImageListItems.GetItem(AIndex: Integer): TJvImageListItem;
begin
  Result := TJvImageListItem(inherited GetItem(AIndex));
end;

{$IFDEF COMPILER5}
function TJvImageListItems.Owner: TPersistent;
begin
  Result := GetOwner;
end;
{$ENDIF COMPILER5}

procedure TJvImageListItems.SetItem(AIndex: Integer; Value: TJvImageListItem);
begin
  inherited SetItem(AIndex, Value);
end;

procedure TJvImageListItems.Update(Item: TCollectionItem);
begin
  if Assigned(Item) then
    TJvImageListItem(Item).UpdateImageList;
end;

{ Loads the bitmaps for the ImageList from the bitmap Bitmap.
  The return value is the number of added bitmaps. }

function LoadImageListFromBitmap(ImgList: TCustomImageList; const Bitmap: TBitmap;
  MaskColor: TColor = clFuchsia; AutoMaskColor: Boolean = False): Integer; overload;
var
  Bmp: TBitmap;
  Width, Height: Integer;
  i: Integer;
  TempImageList: TCustomImageList;
begin
  Result := 0;
  if (ImgList = nil) or (ImgList.Width = 0) or (ImgList.Height = 0) or
    (Bitmap = nil) then
    Exit;

  Width := ImgList.Width;
  Height := ImgList.Height;
  Result := Bitmap.Width div Width; // count
  if (Result = 0) and (Bitmap.Width > 0) then
    Result := 1;
  TempImageList := TCustomImageList.CreateSize(Width, Height);
  Bmp := TBitmap.Create;
  try
    Bmp.PixelFormat := Bitmap.PixelFormat;
    {$IFDEF VCL}
    TempImageList.Handle := CreateImageListHandle(Width, Height,
      Bitmap.PixelFormat, ImgList.Masked, Result);
    {$ENDIF VCL}

   // split Bitmap and add all bitmaps to ImgList
    for i := 0 to Result - 1 do
    begin
      if AutoMaskColor then
        MaskColor := Bitmap.Canvas.Pixels[i * Width, Height - 1];

      Bmp.Canvas.Brush.Color := MaskColor;
      Bmp.Width := 0; // clear bitmap
      Bmp.Width := Width;
      Bmp.Height := Height;
      {$IFDEF VCL}
      BitBlt(Bmp.Canvas.Handle, 0, 0, Width, Height,
        Bitmap.Canvas.Handle, i * Width, 0, SRCCOPY);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      Bmp.Canvas.CopyRect(Rect(0, 0, Width, Height),
        Bitmap.Canvas, Rect(i * Width, 0, (i + 1) * Width, Height));
      {$ENDIF VisualCLX}

      TempImageList.AddMasked(Bmp, MaskColor);
    end;
    ImgList.AddImages(TempImageList);
  finally
    Bmp.Free;
    TempImageList.Free;
  end;
end;

function LoadImageListFromBitmap(ImgList: TCustomImageList; const Bitmap: TBitmap;
  MaskBitmap: TBitmap): Integer; overload;
var
  Bmp, MaskBmp: TBitmap;
  Width, Height: Integer;
  i: Integer;
  TempImageList: TCustomImageList;
begin
  Result := 0;
  if (ImgList = nil) or (ImgList.Width = 0) or (ImgList.Height = 0) or
    (Bitmap = nil) or (MaskBitmap = nil) then
    Exit;

  Width := ImgList.Width;
  Height := ImgList.Height;
  Result := Bitmap.Width div Width; // calc count
  if (Result = 0) and (Bitmap.Width > 0) then
    Result := 1;
  TempImageList := TCustomImageList.CreateSize(Width, Height);
  Bmp := TBitmap.Create;
  MaskBmp := TBitmap.Create;
  try
    Bmp.PixelFormat := Bitmap.PixelFormat;
    MaskBmp.PixelFormat := MaskBitmap.PixelFormat;

    {$IFDEF VCL}
    TempImageList.Handle := CreateImageListHandle(Width, Height,
      Bitmap.PixelFormat, ImgList.Masked, Result);
    {$ENDIF VCL}

   // split Bitmap and add all bitmaps to ImgList
    for i := 0 to Result - 1 do
    begin
      Bmp.Width := 0; // clear bitmap
      Bmp.Width := Width;
      Bmp.Height := Height;
      {$IFDEF VCL}
      BitBlt(Bmp.Canvas.Handle, 0, 0, Width, Height,
        Bitmap.Canvas.Handle, i * Width, 0, SRCCOPY);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      Bmp.Canvas.CopyRect(Rect(0, 0, Width, Height),
        Bitmap.Canvas, Rect(i * Width, 0, (i + 1) * Width, Height));
      {$ENDIF VisualCLX}

      MaskBmp.Width := 0; // clear bitmap
      MaskBmp.Width := Width;
      MaskBmp.Height := Height;
      {$IFDEF VCL}
      BitBlt(MaskBmp.Canvas.Handle, 0, 0, Width, Height,
        MaskBitmap.Canvas.Handle, i * Width, 0, SRCCOPY);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      MaskBmp.Canvas.CopyRect(Rect(0, 0, Width, Height),
        MaskBitmap.Canvas, Rect(i * Width, 0, (i + 1) * Width, Height));
      {$ENDIF VisualCLX}

      TempImageList.Add(Bmp, MaskBmp);
    end;
    ImgList.AddImages(TempImageList);
  finally
    Bmp.Free;
    TempImageList.Free;
  end;
end;

//=== { TJvImageList } =======================================================

destructor TJvImageList.Destroy;
begin
  FItems.Free;
  FPicture.Free;
  FResourceIds.Free;
  inherited Destroy;
end;

procedure TJvImageList.InitializeImageList;
begin
  FModified := False;

  {$IFDEF VCL}
  if not (csDesigning in ComponentState) and not HandleNeededHookInstalled then
    InstallHandleNeededHook;
  {$ENDIF VCL}

  FUpdateLock := 0;

  FMode := imPicture;
  FTransparentMode := tmColor;
  FTransparentColor := clFuchsia;
  {$IFDEF VCL}
  FPixelFormat := pfDevice;
  {$ENDIF VCL}

  FFileName := '';
  FPicture := TPicture.Create;
  FPicture.OnChange := DataChanged;

  FResourceIds := TStringList.Create;
  TStringList(FResourceIds).OnChange := DataChanged;

  FItems := TJvImageListItems.Create(Self);
end;

procedure TJvImageList.Assign(Source: TPersistent);
var
  ImageList: TJvImageList;
begin
  ImageList := TJvImageList(Source);

  BeginUpdate;
  try
    if (Source <> nil) and (Source is TJvImageList) then
    begin
      Clear;
      FMode := imClassic; // lock update

      if (ImageList.Picture.Graphic <> nil) and not ImageList.Picture.Graphic.Empty then
        Picture.Assign(ImageList.Picture)
      else
        Picture.Assign(nil);
      ResourceIds.Assign(ImageList.ResourceIds);
      // Do not assign FileName here.
      TransparentMode := ImageList.TransparentMode;
      TransparentColor := ImageList.TransparentColor;
      {$IFDEF VCL}
      PixelFormat := ImageList.FPixelFormat;
      {$ENDIF VCL}
    end;

    inherited Assign(Source);

    if (Source <> nil) and (Source is TJvImageList) then
      Mode := ImageList.Mode; // enable update
  finally
    EndUpdate;
  end;
end;

procedure TJvImageList.BeginUpdate;
begin
  if FUpdateLock = 0 then
    FModified := False;
  Inc(FUpdateLock);
end;

procedure TJvImageList.EndUpdate;
begin
  Dec(FUpdateLock);
  if (FUpdateLock = 0) and FModified then
    Change;
end;

procedure TJvImageList.Change;
begin
  FModified := True;
  if FUpdateLock = 0 then
    inherited Change;
end;

procedure TJvImageList.DataChanged(Sender: TObject);
begin
  UpdateImageList;
end;

procedure TJvImageList.SetPicture(Value: TPicture);
begin
  if (Value <> FPicture) then
    FPicture.Assign(Value);
end;

procedure TJvImageList.SetTransparentMode(Value: TJvImageListTransparentMode);
begin
  if Value <> FTransparentMode then
  begin
    FTransparentMode := Value;
    UpdateImageList;
  end;
end;

procedure TJvImageList.SetTransparentColor(Value: TColor);
begin
  if Value <> FTransparentColor then
  begin
    FTransparentColor := Value;
    if FTransparentMode = tmColor then
      UpdateImageList;
  end;
end;

procedure TJvImageList.SetFileName(const Value: TFileName);
begin
  if not SameFileName(Value, FFileName) then
  begin
    FFileName := Value;
    DoLoadFromFile;
  end;
end;

procedure TJvImageList.DoLoadFromFile;
begin
  if (not (csDesigning in ComponentState)) and (csLoading in ComponentState) then
    Exit;

  if (FFileName <> '') and FileExists(FFileName)
    {$IFDEF UNIX} and not DirectoryExists(FFileName) {$ENDIF} then
  try
    FPicture.LoadFromFile(FFileName);
  except
    // ignore exception
  end;
end;

procedure TJvImageList.SlicePictureToImageList;
var
  Bmp: TBitmap;
  OwnBitmap: Boolean;
  MaskColor: TColor;
begin
  BeginUpdate;
  try
    Clear;
    if FPicture.Graphic = nil then
      Exit;

    OwnBitmap := False;
    if FPicture.Graphic is TBitmap then
      Bmp := FPicture.Bitmap
    else
    begin
      OwnBitmap := True;
      Bmp := TBitmap.Create;
      Bmp.Canvas.Brush.Color := FTransparentColor;
      Bmp.Width := FPicture.Width;
      Bmp.Height := FPicture.Height;
      Bmp.Canvas.Draw(0, 0, FPicture.Graphic);
    end;
    try
      if TransparentMode = tmNone then
        MaskColor := clNone
      else
        MaskColor := TransparentColor;

      LoadImageListFromBitmap(Self, Bmp, MaskColor, TransparentMode = tmAuto);
    finally
      if OwnBitmap then
        Bmp.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TJvImageList.ResourceIdsToImageList;
var
  Bmp: TBitmap;
  ResStream: TResourceStream;
  i: Integer;
  MaskColor: TColor;
begin
  BeginUpdate;
  try
    Clear;
    if ResourceIds.Count = 0 then
      Exit;

    Bmp := TBitmap.Create;
    try
      for i := 0 to ResourceIds.Count - 1 do
      begin
        if Trim(ResourceIds[i]) <> '' then
        try
         // load resource
          ResStream := nil;
          try
            try
              ResStream := TResourceStream.Create(HInstance, ResourceIds[i], RT_BITMAP);
            except
              ResStream := nil;
            end;
            if ResStream <> nil then
              Bmp.LoadFromResourceName(HInstance, ResourceIds[i])
            else
            begin
              ResStream := TResourceStream.Create(HInstance, ResourceIds[i], RT_RCDATA);
              Bmp.LoadFromStream(ResStream);
            end;
          finally
            ResStream.Free;
          end;

         // add bitmap
          if not Bmp.Empty and (Bmp.Width > 0) and (Bmp.Height > 0) then
          begin
            case TransparentMode of
              tmNone:
                MaskColor := clNone;
              tmColor:
                MaskColor := TransparentColor;
              tmAuto:
                MaskColor := Bmp.Canvas.Pixels[0, Bmp.Height - 1];
            else
              MaskColor := clNone; // make the compiler happy
            end;
            AddMasked(Bmp, MaskColor);
          end;
        except
          // ignore exception
        end;
      end;
    finally
      Bmp.Free;
    end;
  finally
    EndUpdate;
  end;
end;

type
  TComponentAccessProtected = class(TComponent);
  TDefineProperties = procedure(Self: TComponent; Filer: TFiler);

procedure TJvImageList.DefineProperties(Filer: TFiler);
begin
  Inc(FUpdateLock); // no BeginUpdate/EndUpdate here
  try
    if (Filer is TWriter) then
      DoLoadFromFile; // update Picture.Graphic if a filename is specified

    if (Filer is TWriter) and
      (((FMode = imPicture) and (FPicture.Graphic <> nil) and (not FPicture.Graphic.Empty)) or
      ((FMode = imResourceIds) and (FResourceIds.Count > 0)) or
      ((FMode = imItemList) and (FItems.Count > 0))) then
      TDefineProperties(@TComponentAccessProtected.DefineProperties)(Self, Filer)
    else
      inherited DefineProperties(Filer);
  finally
    Dec(FUpdateLock);
  end;
end;

{$IFDEF VCL}
procedure TJvImageList.SetPixelFormat(const Value: TPixelFormat);
var
  ImgList: TJvImageList;
begin
  if (Value <> FPixelFormat) and not (Value in [pf1bit, pfCustom]) then
  begin
    if HandleAllocated then
    begin
      BeginUpdate;
      try
       // convert image list
        ImgList := TJvImageList.CreateSize(Width, Height);
        try
          ImgList.Assign(Self); // copy imagelist with old pixelformat
          FPixelFormat := Value; // set new pixelformat
          CreateImageList; // create new image list handle
          AddImages(ImgList);
        finally
          ImgList.Free;
        end;
      finally
        EndUpdate;
      end;
    end
    else
      FPixelFormat := Value;
  end;
end;
{$ENDIF VCL}

procedure TJvImageList.SetItems(AItems: TJvImageListItems);
begin
  Clear;
  FItems.Assign(AItems);
end;

procedure TJvImageList.AddItem(ABitmap: TBitmap; ATransparentColor: TColor);
var
  BitmapItem: TJvImageListItem;
begin
  if Mode <> imItemList then
    Clear;
  Mode := imItemList;
  BitmapItem := FItems.Add;
  BitmapItem.Kind := ikInlineBitmap;
  BitmapItem.Bitmap.Assign(ABitmap);
  BitmapItem.TransparentColor := ATransparentColor;
end;

procedure TJvImageList.AddItem(const AResourceName: string; ATransparentColor: TColor);
var
  ResourceItem: TJvImageListItem;
begin
  if Mode <> imItemList then
    Clear;
  Mode := imItemList;
  ResourceItem := FItems.Add;
  ResourceItem.Kind := ikResourceBitmap;
  ResourceItem.ResourceName := AResourceName;
  ResourceItem.TransparentColor := ATransparentColor;
end;

procedure TJvImageList.DeleteItem(AIndex: Integer);
begin
  if Mode = imItemList then
    FItems.Delete(AIndex)
  else
    ItemListError;
end;

procedure TJvImageList.ClearItems;
begin
  if Mode = imItemList then
  begin
    Clear;
    FItems.Clear;
  end
  else
    ItemListError;
end;

function TJvImageList.GetItemInfoStr(AIndex: Integer): string;
begin
  Result := '';
  if Mode = imItemList then
    Result := FItems[AIndex].DisplayName
  else
    ItemListError;
end;

procedure TJvImageList.SetResourceIds(Value: TStrings);
begin
  if (Value <> nil) and (Value <> FResourceIds) then
    FResourceIds.Assign(Value);
end;

procedure TJvImageList.SetMode(const Value: TJvImageListMode);
begin
  if Value <> FMode then
  begin
    FMode := Value;
    UpdateImageList;
  end;
end;

procedure TJvImageList.UpdateImageList;
begin
  case FMode of
    imClassic:
      ; // do nothing
    imPicture:
      SlicePictureToImageList;
    imResourceIds:
      ResourceIdsToImageList;
    imItemList:
      ; // do nothing
  end;
end;

{$IFDEF VCL}

procedure TJvImageList.SetInternalHandle(Value: THandle);
begin
  if not HandleAllocated or (Handle <> Value) then
  begin
    Inc(FUpdateLock); // no BeginUpdate/EndUpdate here
    try
      Handle := Value;
    finally
      Dec(FUpdateLock);
    end;
  end;
end;

procedure TJvImageList.HandleNeeded;
begin
  if not HandleAllocated then
    CreateImageList;
end;

procedure TJvImageList.CreateImageList;
begin
  FHandle := CreateImageListHandle(Width, Height, FPixelFormat, Masked, AllocBy);
  if not HandleAllocated then
    raise EInvalidOperation.CreateRes(@SInvalidImageList);
  if BkColor <> clNone then
    BkColor := BkColor;
end;

procedure TJvImageList.DrawIndirect(ImageListDrawParams: TImageListDrawParams);
begin
  ImageListDrawParams.cbSize := SizeOf(ImageListDrawParams);
  ImageListDrawParams.himl := Handle;
  ImageList_DrawIndirect(@ImageListDrawParams);
end;

function TJvImageList.Merge(Index1: Integer; ImageList: TImageList;
  Index2, dx, dy: Integer): TImageList;
var
  h: THandle;
begin
  h := ImageList_Merge(Handle, Index1, ImageList.Handle, Index2, dx, dy);
  if h = 0 then
    Result := nil
  else
  begin
    Result := TJvImageList.Create(nil);
    Result.Handle := h;
  end;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvImageList.GetIcon(Index: Integer; Ico: TIcon);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    GetBitmap(Index, Bmp);
    Ico.Assign(Bmp);
  finally
    Bmp.Free;
  end;
end;
{$ENDIF VisualCLX}



procedure TJvImageList.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvImageList.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

{$IFDEF VCL}

procedure TJvImageList.Initialize;
begin
  inherited Initialize;
  InitializeImageList;
end;

procedure TJvImageList.LoadFromStream(Stream: TStream);
var
  Adapter: IStream;
begin
  Adapter := TStreamAdapter.Create(Stream);
  Handle := ImageList_Read(Adapter);
end;

procedure TJvImageList.SaveToStream(Stream: TStream);
type
  TWriteExProc = function(himl: HIMAGELIST; Flags: Cardinal; Stream: IStream): HResult; stdcall;
const
  ILP_NORMAL = 0;
  ILP_DOWNLEVEL = 1;
var
  Adapter: IStream;
  ImageList_WriteEx: TWriteExProc;
begin
  Adapter := TStreamAdapter.Create(Stream);
  if PixelFormat <> pf32bit then // 32 Bit is only supported by CommCtrls 6.0
  begin
    ImageList_WriteEx := GetProcAddress(GetModuleHandle('comctl32.dll'), 'ImageList_WriteEx');
    if Assigned(ImageList_WriteEx) then
    begin
      // write down
      ImageList_WriteEx(Handle, ILP_DOWNLEVEL, Adapter);
      Exit;
    end;
  end;
  ImageList_Write(Handle, Adapter);
end;

{$ENDIF VCL}

{$IFDEF VisualCLX}

procedure TJvImageList.Initialize(const AWidth, AHeight: Integer);
begin
  inherited Initialize(AWidth, AHeight);
  InitializeImageList;
end;

procedure TJvImageList.LoadFromStream(Stream: TStream);
begin
  ReadData(Stream);
end;

procedure TJvImageList.SaveToStream(Stream: TStream);
begin
  WriteData(Stream);
end;

{$ENDIF VisualCLX}

procedure TJvImageList.ItemListError;
begin
  raise EJvImageListError.CreateResFmt(@RsEWrongImageListMode, ['imItemList']);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF VCL}
  UninstallHandleNeededHook;
  {$ENDIF VCL}
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.

