{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThumbNail.PAS, released on 2002-07-03.

The Initial Developer of the Original Code is John Kozikopulos [Stdreamer att Excite dott com]
Portions created by John Kozikopulos are Copyright (C) 2002 John Kozikopulos.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Thumbimage, ThumbNail components
  Thumbimage is a TImage descentant wich passes the control of the mouse events
  to the ThumbNail and have the ability to change an images look by changing
  the rgb values with the changergb,changergbcurve procedures.
  You can have precise control over the images look.
  The changergb procedure just adds the values you pass to its rgb variables to
  the actual values of the image.
  The Changergbcurves procedure just replaces the value of the rgb values
  accordingly with the values that passed in the the arrays.
  e.g.
  the r array in the position 15 has a value of 35 this meens that wherever in
  the Picture there is a pixels which has a red value equall to 15 it will be ]
  replaced with the value 35.

  ThumbNail is what the name says a component to simply shrink an image
  proportionally to fit in a portion of the screen with some extra mouse handling
  to Create a Button like effect. Just give it a FileName and it will do the work
  for you.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvThumbnails;

{$I jvcl.inc}

interface

uses
  Classes, Controls, ExtCtrls, SysUtils, Messages, Graphics, Windows, Forms,
  JvThumbImage, JvBaseThumbnail, Dialogs;

const
  TH_IMAGESIZECHANGED = WM_USER + 1;

type
  // (rom) elements renamed
  TTitlePos = (tpUp, tpDown, tpNone);

  TTitleNotify = procedure(Sender: TObject; FileName: string;
    var ThumbnailTitle: string) of object;

  TJvThumbnail = class(TJvBaseThumbnail)
  private
    FTitle: string;
    FTitlePanel: TJvThumbTitle;
    FTitleColor: TColor;
    FTitleFont: TFont;
    FStreamFileKind: TGRFKind;
    FDFileCreated: string;
    FDFileChanged: string;
    FDFileAccessed: string;
    FShowTitle: Boolean;
    FDFileSize: Longint;
    FStream: TStream;
    FImageWidth: Longint;
    FImageHeight: Longint;
    FClientHeight: Word;
    FClientWidth: Word;
    FShadowObj: TShape;
    FUpdated: Boolean;
    FImageReady: Boolean;
    FTitlePlacement: TTitlePos;
    FPhotoName: TFileName;
    FPhoto: TJvThumbImage;
    FOnGetTitle: TTitleNotify;
    FMousePressed: Boolean;
    FDestroying: Boolean;
    FAsButton: Boolean;
    FMinimizeMemory: Boolean;
    FAutoLoad: Boolean; // if True then load the image either from a thumb file or Create it from the FileName
    FShadowColor: TColor;
    FShowShadow: Boolean;
    FHShadowOffset: Word;
    FVShadowOffset: Word;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure PhotoOnProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean;
      const R: TRect; const Msg: string);
    procedure RefreshFont(Sender: TObject);
    procedure SetFileName(const AFile: string);
    function LoadFile(AFile: string): string;
    function GetFileName: string;
    procedure CalculateImageSize; virtual;
    procedure SetClientWidth(AWidth: Word);
    procedure SetDummyStr(AStr: string);
    procedure SetMinimizeMemory(Min: Boolean);
    procedure SetDummyCard(AInt: Longint);
    procedure SetClientHeight(AHeight: Word);
    procedure SetShowTitle(const AState: Boolean);
    procedure SetTitlePlacement(const AState: TTitlePos);
    procedure SetTitle(const Value: string);
    procedure SetTitleColor(const Value: TColor);
    procedure SetStream(const AStream: TStream);
    procedure SetTitleFont(const Value: TFont);
    procedure GetFileInfo(AName: string);
    procedure SetShowShadow(Show: Boolean);
//    procedure SetShadowColor(aColor: TColor);
  protected
    procedure THSizeChanged(var Msg: TMessage); message TH_IMAGESIZECHANGED;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DoBoundsChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetTitlePanel(ATitle: string; AFont: TFont; AColor: TColor);
    procedure Refresh;
    property Stream: TStream read FStream write SetStream;
    property Photo: TJvThumbImage read FPhoto write FPhoto;
  published
    property FileName: string read GetFileName write SetFileName;
    property Title: string read FTitle write SetTitle;
    property TitleColor: TColor read FTitleColor write SetTitleColor;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property ImageReady: Boolean read FImageReady;
    property OnGetTitle: TTitleNotify read FOnGetTitle write FOnGetTitle;
    property ClientWidth: Word read FClientWidth write SetClientWidth;
    property ClientHeight: Word read FClientHeight write SetClientHeight;
    { Do not store dummies }
    property FileSize: Longint read FDFileSize write SetDummyCard stored False;
    property FileAccessed: string read FDFileAccessed write SetDummyStr stored False;
    property FileCreated: string read FDFileCreated write SetDummyStr stored False;
    property FileChanged: string read FDFileChanged write SetDummyStr stored False;
    property ImageWidth: Longint read FImageWidth default 0;
    property ImageHeight: Longint read FImageHeight default 0;
    property AsButton: Boolean read FAsButton write FAsButton;
    property MinimizeMemory: Boolean read FMinimizeMemory write SetMinimizeMemory;
    property StreamFileType: TGRFKind read FStreamFileKind write FStreamFileKind;
    property ShowTitle: Boolean read FShowTitle write SetShowTitle;
    property TitlePlacement: TTitlePos read FTitlePlacement write SetTitlePlacement;
    property AutoLoad: Boolean read FAutoLoad write FAutoLoad;
    property ShadowColor: TColor read FShadowColor write FShadowColor;
    property ShowShadow: Boolean read FShowShadow write SetShowShadow;
  end;

implementation

uses
  jpeg,
  JvThumbViews, JvResources;

//uses {$IFNDEF COMPILER6_UP}Gifimage,{$ENDIF} Pcx_unit, Targa, PngImage, jpeg;

constructor TJvThumbnail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPhotoName := TFileName.Create;
  FHShadowOffset := 3;
  FVShadowOffset := 3;
  FShowShadow := False;
  FShadowColor := clSilver;
  FShadowObj := TShape.Create(Self);
  FShadowObj.Visible := FShowShadow;
  FShadowObj.Brush.Color := FShadowColor;
  FShadowObj.Parent := Self;
  FShadowObj.Pen.Style := psClear;
  Photo := TJvThumbImage.Create(Self);
  Photo.AutoSize := False;
  Photo.Align := alNone;
  Photo.Stretch := True;
  Photo.OnProgress := PhotoOnProgress;

  FShadowObj.Width := Photo.Width;
  FShadowObj.Height := Photo.Height;
  FShadowObj.Left := Photo.Left + FHShadowOffset;
  FShadowObj.Top := Photo.Top + FVShadowOffset;
  FTitlePanel := TJvThumbTitle.Create(Self);
  FTitlePanel.Align := alTop;
  FTitlePanel.Height := 15;
  FTitlePanel.Alignment := taCenter;
  FTitleColor := clBtnFace;
  FTitlePanel.Color := FTitleColor;
  FTitleFont := TFont.Create;
  FTitleFont.OnChange := RefreshFont;
  FTitlePanel.BevelOuter := bvLowered;
  FTitlePanel.ParentColor := True;
  FTitlePanel.Color := Self.Color;
  if FTitlePlacement = tpNone then
    FTitlePanel.Visible := False;
  FTitle := '';
  FUpdated := False;
  InsertControl(Photo);
  InsertControl(FTitlePanel);
  Align := alNone;
  if AOwner is TJvThumbView then
  begin
    Width := TJvThumbView(Owner).MaxWidth;
    Height := TJvThumbView(Owner).MaxHeight;
  end
  else
  begin
    Width := 120;
    Height := 120;
  end;
  FMinimizeMemory := True;
  AsButton := False;
  Left := 10;
  Top := 10;
  Visible := True;
  BevelOuter := bvRaised;
  StreamFileType := grBMP;
  FAutoLoad := True;
end;

destructor TJvThumbnail.Destroy;
begin
  FDestroying := True;
  Photo.OnProgress := nil;
  FPhotoName.Free;
  FTitleFont.OnChange := nil;
  FTitleFont.Free;
  inherited Destroy;
end;

procedure TJvThumbnail.SetShowTitle(const AState: Boolean);
begin
  if AState <> FShowTitle then
  begin
    FShowTitle := AState;
    FTitlePanel.Visible := AState;
  end
end;

procedure TJvThumbnail.DoBoundsChanged;
begin
  CalculateImageSize;
  inherited DoBoundsChanged;
end;

procedure TJvThumbnail.SetStream(const AStream: TStream);
var
  Bmp: Graphics.TBitmap;
  Size: TPoint;
  Img2: TJPEGImage;
begin
  case StreamFileType of
    grBMP:
      Photo.Picture.Bitmap.LoadFromStream(AStream);
    grEMF, grWMF:
      Photo.Picture.Metafile.LoadFromStream(AStream);
    grJPG:
      begin
        Img2 := TJPEGImage.Create;
        Img2.LoadFromStream(AStream);
        Photo.Picture.Assign(Img2);
        FreeAndNil(Img2);
      end;
  end;

  if FMinimizeMemory then
  begin
    Bmp := Graphics.TBitmap.Create;
    if Parent is TJvThumbView then
      Size := ProportionalSize(Point(Photo.Picture.Width, Photo.Picture.Height),
        Point(TJvThumbView(Parent).MaxWidth, TJvThumbView(Parent).MaxHeight))
    else
      Size := ProportionalSize(Point(Photo.Picture.Width, Photo.Picture.Height),
        Point(Width, Height));
    Bmp.Width := Size.X;
    Bmp.Height := Size.Y;
    Bmp.handletype := bmDIB;
    Bmp.pixelformat := pf24bit;
    Bmp.Canvas.StretchDraw(rect(0, 0, Bmp.Width, Bmp.Height),
      Photo.Picture.Graphic);
    //Photo.Picture.Graphic.Free; // (rom) not needed
    //Photo.Picture.Graphic := nil;
    Photo.Picture.Assign(Bmp);
    Bmp.Free;
  end;
end;

procedure TJvThumbnail.SetClientWidth(AWidth: Word);
begin
  FClientWidth := (Width - (BorderWidth * 2)) - 8;
end;

procedure TJvThumbnail.SetClientHeight(AHeight: Word);
begin
  if Assigned(FTitlePanel) then
    FClientHeight := Height - (FTitlePanel.Height + 8)
  else
    FClientHeight := Height - 8;
end;

// dummy property functions to allow the object inspector to
// show the properties and their values

procedure TJvThumbnail.SetDummyStr(AStr: string);
begin
end;

procedure TJvThumbnail.SetDummyCard(AInt: Longint);
begin
end;

procedure TJvThumbnail.PhotoOnProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  FImageReady := (Stage = psEnding);
end;

procedure TJvThumbnail.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if AsButton then
    if Button = mbLeft then
    begin
      FMousePressed := True;
      BevelOuter := bvLowered;
      FTitlePanel.BevelOuter := bvRaised;
    end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvThumbnail.SetShowShadow(Show: Boolean);
begin
  FShadowObj.Visible := Show;
  FShowShadow := Show;
end;

{procedure TJvThumbnail.SetShadowColor(aColor: TColor);
begin
  FShadowObj.Brush.Color := aColor;
  FShadowColor := aColor;
end;}

procedure TJvThumbnail.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if AsButton then
    if FMousePressed then
    begin
      if (X < 0) or (X > Width) or (Y < 0) or (Y > Height) then
      begin
        BevelOuter := bvRaised;
        FTitlePanel.BevelOuter := bvLowered
      end
      else
      begin
        BevelOuter := bvLowered;
        FTitlePanel.BevelOuter := bvRaised;
      end;
    end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvThumbnail.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if AsButton then
    if Button = mbLeft then
    begin
      FMousePressed := False;
      BevelOuter := bvRaised;
      FTitlePanel.BevelOuter := bvLowered;
    end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvThumbnail.GetFileInfo(AName: string);
var
  FileInfo: TWin32FindData;
  H: THandle;
  Dft: DWORD;
  Lft: TFileTime;
begin
  H := Windows.FindFirstFile(PChar(AName), FileInfo);
  if H <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(H);
    FileTimeToLocalFileTime(FileInfo.ftLastAccessTime, Lft);
    FileTimeToDosDateTime(Lft, LongRec(Dft).Hi, LongRec(Dft).Lo);
    try
      FDFileAccessed := DateTimeToStr(FileDateToDateTime(Dft));
    except
      FDFileAccessed := RsUnknown;
    end;
    FileTimeToLocalFileTime(FileInfo.ftLastwriteTime, Lft);
    FileTimeToDosDateTime(Lft, LongRec(Dft).Hi, LongRec(Dft).Lo);
    try
      FDFileChanged := DateTimeToStr(FileDateToDateTime(Dft));
    except
      FDFileChanged := RsUnknown;
    end;
    FileTimeToLocalFileTime(FileInfo.ftCreationTime, Lft);
    FileTimeToDosDateTime(Lft, LongRec(Dft).Hi, LongRec(Dft).Lo);
    try
      FDFileCreated := DateTimeToStr(FileDateToDateTime(Dft));
    except
      FDFileCreated := RsUnknown;
    end;
    FDFileSize := (FileInfo.nFileSizeHigh * MAXDWORD) + FileInfo.nFileSizeLow;
  end;
end;

function TJvThumbnail.GetFileName: string;
begin
  Result := FPhotoName.FileName;
end;

function TJvThumbnail.LoadFile(AFile: string): string;
var
  FName: string;
begin
  try
    FName := AFile;
    Photo.LoadFromFile(AFile);
    FImageWidth := Photo.Picture.Width;
    FImageHeight := Photo.Picture.Height;
    FUpdated := False;
    CalculateImageSize;
    Photo.Visible := True;
  except
    // (rom) ShowMessage removed
    FName := '';
  end;
  if MinimizeMemory and (FPhotoName.FileName <> '') then
  begin
    if Owner is TJvThumbView then
      Photo.ScaleDown(TJvThumbView(Owner).MaxWidth, TJvThumbView(Owner).MaxHeight)
    else
      Photo.ScaleDown(Width, Height);
  end;
  Result := FName;
end;

procedure TJvThumbnail.SetFileName(const AFile: string);
var
  FName: string;
//  Pos: Longint;
//  tmp: TJvThumbImage;
//  D1, D2: TdateTime;
begin
  if AFile <> '' then
  begin
    GetFileInfo(AFile);
    if FAutoLoad then
      FName := LoadFile(AFile);
  end
  else
    FName := ''; {}
  if FName = AFile then
    if (Title = ExtractFileName(FPhotoName.FileName)) or (Title = '') then
      Title := ExtractFileName(FName);
  FPhotoName.FileName := FName;
end;

procedure TJvThumbnail.CalculateImageSize;
var
  Percent: Byte;
  TempX, TempY: Single;
begin
  SetClientHeight(15);
  SetClientWidth(15);
  if (Photo.Picture.Width > ClientWidth) or (Photo.Picture.Height > ClientHeight) then
  begin
    TempX := ((ClientWidth) / Photo.Picture.Width) * 100;
    TempY := ((ClientHeight) / Photo.Picture.Height) * 100;
  end
  else
  begin
    TempX := 100;
    TempY := 100;
  end;
  if TempX <= TempY then
    Percent := Trunc(TempX)
  else
    Percent := Trunc(TempY);
  Photo.Width := Trunc((Photo.Picture.Width / 100) * Percent);
  Photo.Height := Trunc((Photo.Picture.Height / 100) * Percent);
  Photo.Left := Trunc(Width / 2 - Photo.Width / 2);
  Photo.Top := (Height div 2) - (Photo.Height div 2);
  case FTitlePlacement of
    tpUp:
      Photo.Top := Photo.Top + (FTitlePanel.Height div 2);
    tpDown:
      Photo.Top := Photo.Top - (FTitlePanel.Height div 2);
  end;
  FShadowObj.SetBounds(Photo.Left + FHShadowOffset, Photo.Top + FVShadowOffset,
    Photo.Width, Photo.Height);
end;

procedure TJvThumbnail.THSizeChanged(var Msg: TMessage);
begin
  CalculateImageSize;
end;

procedure TJvThumbnail.SetTitle(const Value: string);
begin
  if Value <> FTitle then
  begin
    FTitle := Value;
    FTitlePanel.Caption := Value;
  end;
end;

procedure TJvThumbnail.WMPaint(var Msg: TWMPaint);
var
  ThumbnailTitle: string;
begin
  if not FUpdated then
  begin
    ThumbnailTitle := Title;
    if Assigned(FOnGetTitle) then
    begin
      FOnGetTitle(Self, FileName, ThumbnailTitle);
      SetTitle(ThumbnailTitle);
    end
    else
    begin
      if ThumbnailTitle = '' then
        SetTitle(ExtractFileName(FileName))
      else
        SetTitle(ThumbnailTitle);
    end;
    FUpdated := True;
  end;
  inherited;
end;

procedure TJvThumbnail.SetTitleColor(const Value: TColor);
begin
  if Value <> FTitleColor then
  begin
    FTitleColor := Value;
    FTitlePanel.Color := Value;
  end;
end;

procedure TJvThumbnail.SetTitleFont(const Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

procedure TJvThumbnail.RefreshFont(Sender: TObject);
begin
  FTitlePanel.Font.Assign(FTitleFont);
end;

procedure TJvThumbnail.SetTitlePanel(ATitle: string; AFont: TFont;
  AColor: TColor);
begin
  SetTitleFont(AFont);
  SetTitleColor(AColor);
  SetTitle(ATitle);
  FUpdated := True;
end;

procedure TJvThumbnail.SetTitlePlacement(const AState: TTitlePos);
begin
  if AState <> FTitlePlacement then
    case AState of
      tpUp:
        FTitlePanel.Align := alTop;
      tpDown:
        FTitlePanel.Align := alBottom;
      tpNone:
        FTitlePanel.Visible := False;
    end;
  if FTitlePlacement = tpNone then
    FTitlePanel.Visible := True;
  FTitlePlacement := AState;
  CalculateImageSize;
end;

procedure TJvThumbnail.SetMinimizeMemory(Min: Boolean);
begin
  if Assigned(Photo.Picture.Graphic) then
  begin
    if FMinimizeMemory <> Min then
    begin
      if Min then
      begin
        if Owner is TJvThumbView then
          Photo.ScaleDown(TJvThumbView(Owner).MaxWidth, TJvThumbView(Owner).MaxHeight)
        else
          Photo.ScaleDown(Width, Height);
      end
      else
      if FMinimizeMemory then
        Photo.Picture.LoadFromFile(FileName);
      FMinimizeMemory := Min;
    end;
  end
  else
    FMinimizeMemory := Min;
end;

procedure TJvThumbnail.Refresh;
begin
  CalculateImageSize;
  inherited Refresh;
end;

end.

