{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImageList.pas, released on 2003-10-09

The Initial Developers of the Original Code are: Andreas Hausladen <Andreas.Hausladen@gmx.de>
Copyright (c) 2003 Andreas Hausladen
All Rights Reserved.

Contributor(s):

Last Modified: 2003-11-29

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvImageList;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF VCL}
  Graphics, Controls, ImgList, CommCtrl,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QImgList,
  {$ENDIF VisualCLX}
  SysUtils, Classes, JvFinalize; 

type
  TJvImageListMode = (imClassic, imPicture, imResourceIds);
  TJvImageListTransparentMode = (tmNone, tmAuto, tmColor);

  TJvImageList = class(TImageList)
  private
    FUpdateLock: Integer;
    FModified: Boolean;

    FTransparentMode: TJvImageListTransparentMode;
    FTransparentColor: TColor;
    FPicture: TPicture;
    FFileName: TFileName;
    {$IFDEF VCL}
    FPixelFormat: TPixelFormat;
    {$ENDIF VCL}
    FResourceIds: TStringList;
    FMode: TJvImageListMode;
    procedure SetFileName(const Value: TFileName);
    procedure SetPicture(Value: TPicture);
    procedure SetTransparentMode(Value: TJvImageListTransparentMode);
    procedure SetTransparentColor(Value: TColor);
    {$IFDEF VCL}
    procedure SetPixelFormat(const Value: TPixelFormat);
    procedure SetInternalHandle(Value: THandle);
    {$ENDIF VCL}
    function GetResourceIds: TStrings;
    procedure SetResourceIds(Value: TStrings);
    procedure SetMode(const Value: TJvImageListMode);

    procedure SlicePictureToImageList;
    procedure ResourceIdsToImageList;
    procedure DoLoadFromFile;
  protected
    procedure DefineProperties(Filer: TFiler); override;
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
      // Merge creates a new TJvImageList and returns it.
    {$ENDIF VCL}
    procedure SaveToFile(const Filename: string);
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromStream(Stream: TStream); virtual;
  published
    {$IFDEF VCL}
    property PixelFormat: TPixelFormat read FPixelFormat write SetPixelFormat default pfDevice;
      // PixelFormat is the color resolution of the image list. pf1bit and
      // pfCustom are not supported.
      // WARNING: pf32bit works under Windows XP only.
    {$ENDIF VCL}
    property TransparentMode: TJvImageListTransparentMode read FTransparentMode write SetTransparentMode default tmColor;
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
    property ResourceIds: TStrings read GetResourceIds write SetResourceIds;
      // ResourceIds contains the resource ids of the bitmaps to load. Allowed
      // are RCDATA (a bitmap file) and BITMAP. ResourceIds property is only
      // loaded into the image list if Mode is imResourceIds.
    property Mode: TJvImageListMode read FMode write SetMode default imPicture;
      // Mode specifies which property the component should use.
      //   imClassic: be a normal TImageList
      //   imPicture: split the image in Picture
      //   imResourceIds: load the images by ResourceIds
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
  {$IFDEF VCL}
  Consts, ActiveX,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QConsts,
  {$ENDIF VisualCLX}
  JvJVCLUtils;

const
  sUnitName = 'JvImageList';

{$IFDEF LINUX}
const
  RT_RCDATA = PChar(1);
{$ENDIF LINUX}


{$IFDEF VCL}

{------------------------------------------------------------------------------}
{ Here we inject a jump to our HandleNeededHook into the static
  TCustomImageList.HandleNeeded method. }

type
  TCustomImageListHack = class(TCustomImageList);

  // we need direct access to the FHandle field because the Handle property
  // calls the Changed method that calls HandleNeeded that calls SetHandle, ...
  TImageListPrivate = class(TComponent)
  protected
    FHeight: Integer;
    FWidth: Integer;
    FAllocBy: Integer;
    FHandle: HImageList;
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
        raise EInvalidOperation.Create(SInvalidImageList);
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
    OrgProc := @TCustomImageListHack.HandleNeeded;

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
    OrgProc := @TCustomImageListHack.HandleNeeded;
    NewProc := @HandleNeededHook;

    Code.Jump := $e9;
    Code.Offset := Integer(NewProc) - Integer(OrgProc) - SizeOf(Code);

    if ReadProcessMemory(GetCurrentProcess, OrgProc, @SavedNeededHookCode, SizeOf(SavedNeededHookCode), n) then
      if WriteProcessMemory(GetCurrentProcess, OrgProc, @Code, SizeOf(Code), n) then
      begin
        HandleNeededHookInstalled := True;
        FlushInstructionCache(GetCurrentProcess, OrgProc, SizeOf(Code));
        AddFinalizeProc(sUnitName, UninstallHandleNeededHook);
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

{ TJvImageList }

procedure TJvImageList.Initialize;
begin
  inherited;
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
  FResourceIds.OnChange := DataChanged;
end;

destructor TJvImageList.Destroy;
begin
  FPicture.Free;
  FResourceIds.Free;
  inherited Destroy;
end;

procedure TJvImageList.Assign(Source: TPersistent);
var ImageList: TJvImageList;
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
      // Do not assign Filename here.
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
  {$IFDEF MSWINDOWS}
  if not SameText(Value, FFileName) then
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  if Value <> FFileName then
  {$ENDIF LINUX}
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
   {$IFDEF LINUX} and not DirectoryExists(FFileName) {$ENDIF} then
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
              tmNone: MaskColor := clNone;
              tmColor: MaskColor := TransparentColor;
              tmAuto: MaskColor := Bmp.Canvas.Pixels[0, Bmp.Height - 1];
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
  TOpenComponent = class(TComponent);
  TDefineProperties = procedure(Self: TComponent; Filer: TFiler);

procedure TJvImageList.DefineProperties(Filer: TFiler);
begin
  Inc(FUpdateLock); // no BeginUpdate/EndUpdate here
  try
    if (Filer is TWriter) then
      DoLoadFromFile; // update Picture.Graphic if a filename is specified

    if (Filer is TWriter) and (FPicture.Graphic <> nil) and
       (not FPicture.Graphic.Empty) and (FMode <> imClassic) then
      TDefineProperties(@TOpenComponent.DefineProperties)(Self, Filer)
    else
      inherited DefineProperties(Filer);
  finally
    Dec(FUpdateLock);
  end;
end;

{$IFDEF VCL}
procedure TJvImageList.SetPixelFormat(const Value: TPixelFormat);
var ImgList: TJvImageList;
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
          ImgList.Assign(Self);   // copy imagelist with old pixelformat
          FPixelFormat := Value;  // set new pixelformat
          CreateImageList;        // create new image list handle
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

function TJvImageList.GetResourceIds: TStrings;
begin
  Result := FResourceIds;
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
    imClassic: ; // do nothing
    imPicture: SlicePictureToImageList;
    imResourceIds: ResourceIdsToImageList;
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
  if not HandleAllocated then CreateImageList;
end;

procedure TJvImageList.CreateImageList;
begin
  FHandle := CreateImageListHandle(Width, Height, FPixelFormat, Masked, AllocBy);
  if not HandleAllocated then
    raise EInvalidOperation.Create(SInvalidImageList);
  if BkColor <> clNone then
    BkColor := BkColor;
end;

procedure TJvImageList.DrawIndirect(ImageListDrawParams: TImageListDrawParams);
begin
  ImageListDrawParams.cbSize := SizeOf(ImageListDrawParams);
  ImageListDrawParams.himl := Handle;
  ImageList_DrawIndirect(@ImageListDrawParams);
end;

function TJvImageList.Merge(Index1: Integer; ImageList: TImageList; Index2,
  dx, dy: Integer): TImageList;
var h: THandle;
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


procedure TJvImageList.LoadFromFile(const Filename: string);
var Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvImageList.SaveToFile(const Filename: string);
var Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

{$IFDEF VCL}

procedure TJvImageList.LoadFromStream(Stream: TStream);
var Adapter: IStream;
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
  if PixelFormat <> pf32Bit then // 32 Bit is only supported by CommCtrls 6.0
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

procedure TJvImageList.LoadFromStream(Stream: TStream);
begin
  ReadData(Stream);
end;

procedure TJvImageList.SaveToStream(Stream: TStream);
begin
  WriteData(Stream);
end;

{$ENDIF VisualCLX}

{$IFDEF VCL}

initialization

finalization
  FinalizeUnit(sUnitName);

{$ENDIF VCL}

end.
