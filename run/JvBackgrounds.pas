{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBackgrounds.PAS, released on 2004-04-26.

The Initial Developer of the Original Code is Robert Rossmair [Robert dott Rossmair att t-online dott de]
Portions created by Robert Rossmair are Copyright (C) 2003 Robert Rossmair.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBackgrounds;

{$I jedi.inc}
{$I windowsonly.inc}

interface

{***************** Conditional Compiler Symbols ************************

 JVCL           JEDI VCL installed (http://sourceforge.net/projects/jvcl/)

 USE_JvGIF      use TGIFImage class from JVCL

 USE_AM_GIF     use GIFImage library by Anders Melander et alii
                (download address:
                 http://www.melander.dk/delphi/gifimage/index.html).

 NO_DESIGNHOOK  Disables visual feedback in design mode.
                $DEFINE this if you experience problems in design mode.
                Such problems might occur if there are other components
                manipulating the TrrBackgrounds.Client's window
                procedure.
 *********************************************************************** }

{.$DEFINE JVCL}
{.$DEFINE USE_AM_GIF}

{$IFDEF USEJVCL}
{$DEFINE USE_JvGIF}
{$ENDIF USEJVCL}

{$IFDEF RECOGNIZE_GIF}
{$DEFINE USE_AM_GIF}
{$UNDEF RECOGNIZE_GIF}
{$ENDIF RECOGNIZE_GIF}

{$IFDEF USE_AM_GIF}
{$UNDEF USE_JvGIF}
{$ENDIF USE_AM_GIF}

uses
  Windows, Messages, Contnrs, Graphics, Controls, Forms, Classes,
  JclGraphUtils,
  JvTypes;

type
  TJvBackgroundMode = (bmTile, bmCenter, bmTopLeft, bmTop, bmTopRight, bmLeft,
    bmBottomLeft, bmRight, bmBottom, bmBottomRight, bmStretch);
  EJvBackgroundError = class(EJVCLException);
  TJvBackgroundShiftMode = (smRows, smColumns);

  TJvBackgroundImage = class(TPersistent)
  private
    FPicture: TPicture;
    FCanvas: TCanvas;
    FHorzOffset: Integer;
    FVertOffset: Integer;
    FOnChange: TNotifyEvent;
    FWorkingBmp: TBitmap;
    FInUpdWorkingBmp: Boolean;
    FMode: TJvBackgroundMode;
    FTransparent: Boolean;
    FTransparentMode: TTransparentMode;
    FTransparentColor: TColor;
    FTileWidth: Integer;
    FTileHeight: Integer;
    FShift: Integer;
    FShiftMode: TJvBackgroundShiftMode;
    FZigZag: Boolean;
    FAutoSizeTile: Boolean;
    FFitPictureSize: Boolean;
    FEnabled: Boolean;
    FPictureValid: Boolean;
    FGrayMapped: Boolean;
    procedure SetGrayMapped(Value: Boolean);
    procedure SysColorChange;
    class function MainWindowHook(var Msg: TMessage): Boolean;
    procedure HookMainWindow;
    procedure UnhookMainWindow;
    procedure Changed;
    function GetTransparentColor: TColor;
    procedure PictureChanged(Sender: TObject);
    procedure SetAutoSizeTile(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetFitPictureSize(Value: Boolean);
    procedure SetMode(Value: TJvBackgroundMode);
    procedure SetPicture(Value: TPicture);
    procedure SetShift(Value: Integer);
    procedure SetShiftMode(Value: TJvBackgroundShiftMode);
    procedure SetTileWidth(Value: Integer);
    procedure SetTileHeight(Value: Integer);
    procedure SetTransparent(Value: Boolean);
    procedure SetTransparentColor(Value: TColor);
    procedure SetTransparentMode(Value: TTransparentMode);
    procedure SetZigZag(Value: Boolean);
    procedure TileGraphic(AClient: TControl; Graphic: TGraphic);
    function TransparentColorStored: Boolean;
    procedure UpdateWorkingBmp;
    procedure WorkingBmpNeeded;
  protected
    function HandleWMEraseBkgnd(AClient: TWinControl; var Msg: TMessage): Boolean;
    function HandleWMPaint(AClient: TWinControl; var Msg: TMessage): Boolean;
    procedure PaintGraphic(AClient: TControl; DC: HDC; Graphic: TGraphic);
    property Canvas: TCanvas read FCanvas;
    property WorkingBmp: TBitmap read FWorkingBmp;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function PaintBackground(AClient: TWinControl; DC: HDC): Boolean;
  published
    property AutoSizeTile: Boolean read FAutoSizeTile write SetAutoSizeTile
      default True;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property FitPictureSize: Boolean
      read FFitPictureSize write SetFitPictureSize default False;
    property GrayMapped: Boolean read FGrayMapped write SetGrayMapped default False;
    property Mode: TJvBackgroundMode read FMode write SetMode default bmTile;
    property Picture: TPicture read FPicture write SetPicture;
    property TileWidth: Integer read FTileWidth write SetTileWidth;
    property TileHeight: Integer read FTileHeight write SetTileHeight;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property TransparentColor: TColor read GetTransparentColor
      write SetTransparentColor stored TransparentColorStored;
    property TransparentMode: TTransparentMode read FTransparentMode
      write SetTransparentMode default tmAuto;
    property Shift: Integer read FShift write SetShift default 0;
    property ShiftMode: TJvBackgroundShiftMode read FShiftMode write SetShiftMode default smRows;
    property ZigZag: Boolean read FZigZag write SetZigZag default False;
  end;

  TJvControlBackground = class(TJvBackgroundImage)
  private
    FClient: TWinControl;
  public
    function HookBeforeMessage(var Msg: TMessage): Boolean;
    procedure HookAfterMessage(var Msg: TMessage);
    constructor Create(AClient: TWinControl);
  end;

  TJvBackground = class;

  TJvBackgroundClientLink = class(TObject)
  private
    FBackground: TJvBackground;
    FClient: TWinControl;
    FNewWndProc: Pointer;
    FPrevWndProc: TFarProc;
    FClientIsMDIForm: Boolean;
    procedure ClientInvalidate;
    procedure MainWndProc(var Msg: TMessage);
    procedure ClientWndProc(var Message: TMessage);
    procedure ForceClient(Value: TWinControl; Force: Boolean = True);
    procedure HookClient;
    procedure UnhookClient;
    function GetClientColor: TColor;
    function GetClientHandle: HWND;
    procedure SetClient(Value: TWinControl);
  protected
    procedure Release;
    property Background: TJvBackground read FBackground;
    property ClientColor: TColor read GetClientColor;
    property ClientHandle: HWND read GetClientHandle;
    property Client: TWinControl read FClient write SetClient;
    property ClientIsMDIForm: Boolean read FClientIsMDIForm;
  public
    constructor Create(ABackground: TJvBackground; AClient: TWinControl);
    destructor Destroy; override;
  end;

  TJvBackgroundClients = class(TPersistent)
  private
    FBackground: TJvBackground;
    FLinks: TObjectList;
    function GetClient(Index: Integer): TWinControl;
    procedure Invalidate;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure FixupReferences(Root: TComponent);
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    function GetLink(Index: Integer): TJvBackgroundClientLink;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    property Background: TJvBackground read FBackground;
    property Links[Index: Integer]: TJvBackgroundClientLink read GetLink;
  public
    constructor Create(ABackground: TJvBackground);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Control: TWinControl);
    procedure Remove(Control: TWinControl);
    function IndexOf(Control: TWinControl): Integer;
    property Clients[Index: Integer]: TWinControl read GetClient; default;
  end;

  TJvBackground = class(TComponent)
  private
    FClients: TJvBackgroundClients;
    FHandle: HWND;
    FImage: TJvBackgroundImage;
    procedure SetClients(Value: TJvBackgroundClients);
    procedure WallpaperChanged(Sender: TObject);
    procedure WndProc(var Msg: TMessage);
    procedure SetImage(const Value: TJvBackgroundImage);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HasClient(Control: TWinControl): Boolean;
  published
    property Image: TJvBackgroundImage read FImage write SetImage;
    property Clients: TJvBackgroundClients read FClients write SetClients;
  end;

procedure GetMappedGrays(var Shades: array of TColor; StartIntensity: Byte);

implementation

uses
  SysUtils, jpeg,
  StdCtrls, CommCtrl, ComCtrls, Dialogs,
  {$IFDEF USE_AM_GIF}
  GIFImage,
  {$DEFINE RECOGNIZE_GIF}
  {$ENDIF USE_AM_GIF}
  {$IFDEF USE_JvGIF}
  JvGIF,
  {$DEFINE RECOGNIZE_GIF}
  {$ENDIF USE_JvGIF}
  JvResources, JvFinalize;

const
  sUnitName = 'JvBackgrounds';

type
  TWinControlAccessProtected = class(TWinControl);

  {$IFDEF USE_JvGIF}
  // make TJvGIFImage's Bitmap property visible
  TGIFImage = class(TJvGIFImage);
  {$ENDIF USE_JvGIF}

const
  ScrollLineSize = 3;
  ScrollUnit = 8;

  CM_RECREATEWINDOW = CM_BASE + 82;
  CM_RELEASECLIENTLINK = CM_BASE + 83;

type
  TColorGradation = array [Byte] of TColor;
  PColorGradation = ^TColorGradation;

var
  SysColorGradation: TColorGradation;
  SysColorGradationInitialized: Boolean = False;
  Hooked: TList = nil;
  Backgrounds: TList = nil;

procedure UpdateSysColorGradation;
var
  SysHLS: THLSVector;
  FaceLum, MaxLum: THLSValue;
  I: Integer;
begin
  SysHLS := RGBtoHLS(ColorToRGB(clBtnHighlight));
  MaxLum := SysHLS.Luminance;
  SysHLS := RGBtoHLS(ColorToRGB(clBtnFace));
  FaceLum := SysHLS.Luminance;
  with SysHLS do
  begin
    for I := 0 to 192 do
    begin
      Luminance := I * FaceLum div 192;
      SysColorGradation[I] := HLStoRGB(Hue, Luminance, Saturation);
    end;
    for I := 193 to 255 do
    begin
      Luminance := FaceLum + (MaxLum - FaceLum) * (I - 192) div (255 - 192);
      SysColorGradation[I] := HLStoRGB(Hue, Luminance, Saturation);
    end;
  end;
end;

procedure SysColorsNeeded;
begin
  if not SysColorGradationInitialized then
  begin
    SysColorGradationInitialized := True;
    UpdateSysColorGradation;
  end;
end;

procedure GetMappedGrays(var Shades: array of TColor; StartIntensity: Byte);
var
  I, Intensity: Integer;
begin
  SysColorsNeeded;
  Intensity := StartIntensity;
  for I := Low(Shades) to High(Shades) do
  begin
    Shades[I] := SysColorGradation[Intensity];
    if Intensity < High(SysColorGradation) then
      Inc(Intensity);
  end;
end;

procedure MapGrays(Dest: TBitmap; Source: TGraphic);
var
  Grays: PColorGradation;
  I: Integer;
  SrcWasTransparent: Boolean;
begin
  if Source = nil then
    Exit;
  New(Grays);
  try
    for I := Low(Grays^) to High(Grays^) do
      Grays[I] := RGB(I, I, I);
    with Dest do
    begin
      if ((Source is TBitmap) and (TBitmap(Source).PixelFormat in [pf1bit..pf8bit]))
        {$IFDEF RECOGNIZE_GIF} or (Source is TGIFImage) {$ENDIF} then
        Assign(Source)
      else
      begin
        PixelFormat := pf8bit;
        Width := Source.Width;
        Height := Source.Height;
        SetBitmapColors(Dest, Grays^, 0);
        SrcWasTransparent := Source.Transparent;
        try
          Source.Transparent := False;
          Canvas.Draw(0, 0, Source);
        finally
          Source.Transparent := SrcWasTransparent;
        end;
      end;
      Handle := CreateMappedBmp(Handle, Grays^, SysColorGradation);
    end;
  finally
    Dispose(Grays);
  end;
end;

function TrimmedOffset(Offset, TileDim: Integer): Integer;
begin
  if TileDim <> 0 then
    if Offset > 0 then
      Offset := (Offset mod TileDim) - TileDim
    else
    if Offset < 0 then
      Dec(Offset, (Offset div TileDim) * TileDim);
  Result := Offset;
end;

function GetClientHandle(AClient: TWinControl): HWND;
begin
  Result := 0;
  if AClient is TCustomForm then
    Result := TForm(AClient).ClientHandle;
  if Result = 0 then
    if AClient.HandleAllocated then
      Result := AClient.Handle;
end;

function GetClientRect(AClient: TControl): TRect;
var
  MDIClientHandle: HWND;
begin
  if AClient is TCustomForm then
  begin
    MDIClientHandle := TForm(AClient).ClientHandle;
    if MDIClientHandle <> 0 then
    begin
      Windows.GetClientRect(MDIClientHandle, Result);
      Exit;
    end;
  end;
  Result := AClient.ClientRect;
end;

function GetVirtualClientRect(AClient: TControl): TRect;
var
  ClientHandle: HWND;
  ScrollInfo: TScrollInfo;
  R: TRect;
  TVTopItem: TTreeNode;
begin
  Result := GetClientRect(AClient);
  if AClient is TWinControl then
  begin
    ClientHandle := TWinControl(AClient).Handle;
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL;
    GetScrollInfo(ClientHandle, SB_HORZ, ScrollInfo);
    if ScrollInfo.nPage > 0 then // horizontal scroll bar visible
    begin
      if ScrollInfo.nMax > Result.Right then
        Result.Right := ScrollInfo.nMax;
      Dec(Result.Left, ScrollInfo.nPos);
      Dec(Result.Right, ScrollInfo.nPos);
    end;
    GetScrollInfo(ClientHandle, SB_VERT, ScrollInfo);
    if ScrollInfo.nPage > 0 then // vertical scroll bar visible
    begin
      if AClient is TCustomListBox then
        with TListBox(AClient) do
        begin
          ScrollInfo.nPos := ScrollInfo.nPos * ItemHeight;
          ScrollInfo.nMax := ScrollInfo.nMax * ItemHeight;
        end
      else
      if AClient is TCustomTreeView then
      begin
        TVTopItem := TCustomTreeView(AClient).TopItem;
        if Assigned(TVTopItem) and TreeView_GetItemRect(ClientHandle, TVTopItem.ItemID, R, False) then
        begin
          ScrollInfo.nPos := ScrollInfo.nPos * R.Bottom;
          ScrollInfo.nMax := ScrollInfo.nMax * R.Bottom;
        end;
      end;
      if ScrollInfo.nMax > Result.Bottom then
        Result.Bottom := ScrollInfo.nMax;
      Dec(Result.Top, ScrollInfo.nPos);
      Dec(Result.Bottom, ScrollInfo.nPos);
    end;
  end;
end;

function GetClientBrush(AClient: TControl): TBrush;
begin
  if AClient is TWinControl then
    Result := TWinControl(AClient).Brush
  else
    Result := AClient.Parent.Brush;
end;

function IsMDIForm(Control: TControl): Boolean;
begin
  Result := False;
  if Assigned(Control) then
    if Control is TCustomForm then
      Result := TForm(Control).FormStyle = fsMDIForm;
end;

//=== { TJvBackgroundImage } =================================================

constructor TJvBackgroundImage.Create;
begin
  inherited Create;
  FCanvas := TCanvas.Create;
  FAutoSizeTile := True;
  FEnabled := True;
  FTransparentColor := clDefault;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  HookMainWindow;
end;

destructor TJvBackgroundImage.Destroy;
begin
  UnhookMainWindow;
  FPicture.Free;
  FWorkingBmp.Free;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvBackgroundImage.Assign(Source: TPersistent);
var
  Src: TJvBackgroundImage;
begin
  if Source is TJvBackgroundImage then
  begin
    Src := TJvBackgroundImage(Source);
    AutoSizeTile := Src.AutoSizeTile;
    Enabled := Src.Enabled;
    FitPictureSize := Src.FitPictureSize;
    GrayMapped := Src.GrayMapped;
    Mode := Src.Mode;
    Picture := Src.Picture;
    TileWidth := Src.TileWidth;
    TileHeight := Src.TileHeight;
    Transparent := Src.Transparent;
    TransparentColor := Src.TransparentColor;
    TransparentMode := Src.TransparentMode;
    Shift := Src.Shift;
    ShiftMode := Src.ShiftMode;
    ZigZag := Src.ZigZag;
  end
  else
    inherited Assign(Source);
end;

procedure TJvBackgroundImage.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TJvBackgroundImage.HandleWMEraseBkgnd(AClient: TWinControl; var Msg: TMessage): Boolean;
begin
  Result := FEnabled and FPictureValid;
  if Result then
  begin
    if not IsIconic(AClient.Handle) then
      if not TWinControlAccessProtected(AClient).FDoubleBuffered or (Msg.wParam = Msg.lParam) then
        PaintBackground(AClient,
          TWMEraseBkgnd(Msg).DC);
    Msg.Result := 1;
  end;
end;

function TJvBackgroundImage.HandleWMPaint(AClient: TWinControl; var Msg: TMessage): Boolean;
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  ClientRect: TRect;
begin
  Result := False;
  if FEnabled and FPictureValid then
    if TWinControlAccessProtected(AClient).FDoubleBuffered and (TWMPaint(Msg).DC = 0) then
    begin
      DC := GetDC(0);
      ClientRect := AClient.ClientRect;
      MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
      ReleaseDC(0, DC);
      MemDC := CreateCompatibleDC(0);
      OldBitmap := SelectObject(MemDC, MemBitmap);
      try
        DC := BeginPaint(AClient.Handle, PS);
        //AClient.Perform(WM_ERASEBKGND, MemDC, MemDC);
        PaintBackground(AClient, MemDC);
        Msg.Result := AClient.Perform(WM_PAINT, MemDC, 0);
      {TWMPaint(Msg).DC := MemDC;
      AClient.Dispatch(Msg);
      TWMPaint(Msg).DC := 0;}
        BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
        EndPaint(AClient.Handle, PS);
      finally
        SelectObject(MemDC, OldBitmap);
        DeleteDC(MemDC);
        DeleteObject(MemBitmap);
      end;
      Result := True;
    end;
end;

procedure TJvBackgroundImage.TileGraphic(AClient: TControl; Graphic: TGraphic);
var
  I, J: Integer;
  iMin: Integer;
  FirstVisibleRow, S, OddShift: Integer;
  Left, Top, Width, Height: Integer;
  HorzOffset, VertOffset: Integer;
begin
  with GetClientRect(AClient) do
  begin
    Width := Right;
    Height := Bottom;
  end;
  if IsMDIForm(AClient) then
  begin
    HorzOffset := FHorzOffset;
    VertOffset := FVertOffset;
  end
  else
    with GetVirtualClientRect(AClient) do
    begin
      HorzOffset := Left;
      VertOffset := Top;
    end;
  if FShiftMode = smRows then
  begin
    FirstVisibleRow := -VertOffset div FTileHeight;
    if VertOffset > 0 then
      Dec(FirstVisibleRow);
  end
  else
  begin
    FirstVisibleRow := -HorzOffset div FTileWidth;
    if HorzOffset > 0 then
      Dec(FirstVisibleRow);
  end;
  Left := TrimmedOffset(HorzOffset, FTileWidth);
  Top := TrimmedOffset(VertOffset, FTileHeight);
  Dec(Width, Left);
  Dec(Height, Top);

  OddShift := 0; // just to satisfy the compiler
  if FShiftMode = smRows then
  begin
    if FZigZag then
    begin
      OddShift := FTileWidth div 2;
      if Odd(FirstVisibleRow) then
        S := OddShift
      else
        S := 0;
    end
    else
    begin
      S := (FirstVisibleRow * FShift) mod FTileWidth;
      if S < 0 then
        Inc(S, FTileWidth);
    end;
    for J := 0 to (Height - 1) div FTileHeight do
    begin
      if S = 0 then
        iMin := 0
      else
        iMin := -1;
      for I := iMin to (Width - 1) div FTileWidth do
        Canvas.Draw(Left + I * FTileWidth + S, Top + J * FTileHeight, Graphic);
      if FZigZag then
        S := S xor OddShift
      else
      begin
        Inc(S, FShift);
        S := S mod FTileWidth;
      end;
    end;
  end
  else
  begin
    if FZigZag then
    begin
      OddShift := FTileHeight div 2;
      if Odd(FirstVisibleRow) then
        S := OddShift
      else
        S := 0;
    end
    else
    begin
      S := (FirstVisibleRow * FShift) mod FTileHeight;
      if S < 0 then
        Inc(S, FTileHeight);
    end;
    for I := 0 to (Width - 1) div FTileWidth do
    begin
      if S = 0 then
        iMin := 0
      else
        iMin := -1;
      for J := iMin to (Height - 1) div FTileHeight do
        Canvas.Draw(Left + I * FTileWidth, Top + J * FTileHeight + S, Graphic);
      if FZigZag then
        S := S xor OddShift
      else
      begin
        Inc(S, FShift);
        S := S mod FTileHeight;
      end;
    end;
  end;
end;

procedure TJvBackgroundImage.PaintGraphic(AClient: TControl; DC: HDC; Graphic: TGraphic);
var
  R, Rg: TRect;
  X, Y, W, H: Integer;
  SaveIndex: Integer;
  WindowStyle: DWORD;
  GraphW, GraphH: Integer;
  Factor, FactorVert: Single;
begin
  SaveIndex := SaveDC(DC);
  with Canvas do
  begin
    Handle := DC;
    if FMode = bmTile then
      TileGraphic(AClient, Graphic)
    else
    begin
      if IsMDIForm(AClient) then
      begin
        R := GetClientRect(AClient);
        // We don't want the background move
        // when scrollbars appear or disappear:
        WindowStyle := GetWindowLong(TForm(AClient).ClientHandle, GWL_STYLE);
        if (WindowStyle and WS_HSCROLL) <> 0 then
          Inc(R.Bottom, GetSystemMetrics(SM_CYHSCROLL));
        if (WindowStyle and WS_VSCROLL) <> 0 then
          Inc(R.Right, GetSystemMetrics(SM_CXVSCROLL));
      end
      else
        R := GetVirtualClientRect(AClient);
      W := R.Right - R.Left;
      H := R.Bottom - R.Top;
      GraphW := Graphic.Width;
      GraphH := Graphic.Height;
      if FFitPictureSize and not (FMode = bmStretch) then
      begin
        Factor := W / GraphW;
        FactorVert := H / GraphH;
        if FactorVert < Factor then
          Factor := FactorVert;
        GraphW := Round(Factor * GraphW);
        GraphH := Round(Factor * GraphH);
      end;
      Rg := Rect(0, 0, GraphW, GraphH);
      Brush := GetClientBrush(AClient);
      case FMode of
        bmCenter:
          begin
            X := R.Left + (W - GraphW) div 2;
            Y := R.Top + (H - GraphH) div 2;
            FillRect(Rect(R.Left, R.Top, R.Right, Y));
            FillRect(Rect(R.Left, Y, X, Y + GraphH));
            FillRect(Rect(X + GraphW, Y, R.Right, Y + GraphH));
            FillRect(Rect(R.Left, Y + GraphH, R.Right, R.Bottom));
            OffsetRect(Rg, X, Y);
          end;
        bmStretch:
          Rg := R;
        bmTopLeft:
          begin
            FillRect(Rect(R.Left + GraphW, R.Top, R.Right, R.Top + GraphH));
            FillRect(Rect(R.Left, R.Top + GraphH, R.Right, R.Bottom));
            OffsetRect(Rg, R.Left, R.Top);
          end;
        bmTopRight:
          begin
            FillRect(Rect(R.Left, R.Top, R.Right - GraphW, R.Top + GraphH));
            FillRect(Rect(R.Left, R.Top + GraphH, R.Right, R.Bottom));
            OffsetRect(Rg, R.Right - GraphW, R.Top);
          end;
        bmBottomLeft:
          begin
            FillRect(Rect(R.Left, R.Top, R.Right, R.Bottom - GraphH));
            FillRect(Rect(R.Left + GraphW, R.Bottom - GraphH, R.Right, R.Bottom));
            OffsetRect(Rg, R.Left, R.Bottom - GraphH);
          end;
        bmBottomRight:
          begin
            FillRect(Rect(R.Left, R.Top, R.Right, R.Bottom - GraphH));
            FillRect(Rect(R.Left, R.Bottom - GraphH, R.Right - GraphW, R.Bottom));
            OffsetRect(Rg, R.Right - GraphW, R.Bottom - GraphH);
          end;
        bmTop:
          begin
            X := R.Left + (W - GraphW) div 2;
            FillRect(Rect(R.Left, R.Top, X, GraphH));
            FillRect(Rect(X + GraphW, R.Top, R.Right, GraphH));
            FillRect(Rect(R.Left, R.Top + GraphH, R.Right, R.Bottom));
            OffsetRect(Rg, X, R.Top);
          end;
        bmLeft:
          begin
            Y := R.Top + (H - GraphH) div 2;
            FillRect(Rect(R.Left, R.Top, R.Right, Y));
            FillRect(Rect(R.Left + GraphW, Y, R.Right, Y + GraphH));
            FillRect(Rect(R.Left, Y + GraphH, R.Right, R.Bottom));
            OffsetRect(Rg, R.Left, Y);
          end;
        bmBottom:
          begin
            X := R.Left + (W - GraphW) div 2;
            Y := R.Bottom - GraphH;
            FillRect(Rect(R.Left, R.Top, R.Right, Y));
            FillRect(Rect(R.Left, Y, X, R.Bottom));
            FillRect(Rect(X + GraphW, Y, R.Right, R.Bottom));
            OffsetRect(Rg, X, Y);
          end;
        bmRight:
          begin
            X := R.Right - GraphW;
            Y := R.Top + (H - GraphH) div 2;
            FillRect(Rect(R.Left, R.Top, R.Right, Y));
            FillRect(Rect(R.Left, Y, X, Y + GraphH));
            FillRect(Rect(R.Left, Y + GraphH, R.Right, R.Bottom));
            OffsetRect(Rg, X, Y);
          end;
      end;
      StretchDraw(Rg, Graphic);
    end;
    Handle := 0;
  end;
  RestoreDC(DC, SaveIndex);
end;

function TJvBackgroundImage.PaintBackground(AClient: TWinControl; DC: HDC): Boolean;
var
  Graphic: TGraphic;
  Bmp: TBitmap;
begin
  Result := FPictureValid and AClient.HandleAllocated;
  if Result then
  begin
    Bmp := nil;
    try
      Graphic := FWorkingBmp;
      if Graphic = nil then
        Graphic := FPicture.Graphic
      else
      if Transparent then
      begin
        Bmp := TBitmap.Create;
        Bmp.Assign(Graphic);
        Bmp.Canvas.Brush := GetClientBrush(AClient);
        Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
        Bmp.Canvas.Draw(0, 0, Graphic);
        Bmp.Transparent := False;
        Graphic := Bmp;
      end;
      PaintGraphic(AClient, DC, Graphic);
    finally
      Bmp.Free;
    end;
  end;
end;

function TJvBackgroundImage.GetTransparentColor: TColor;
var
  Bmp: TBitmap;
begin
  Bmp := nil;
  if FTransparentColor = clDefault then
    {$IFDEF RECOGNIZE_GIF}
    if FPicture.Graphic is TGIFImage then
      Bmp := TGIFImage(FPicture.Graphic).Bitmap
    else
    {$ENDIF RECOGNIZE_GIF}
    if FPicture.Graphic is TBitmap then
      Bmp := TBitmap(FPicture.Graphic);
  if Assigned(Bmp) then
  begin
    if Bmp.Monochrome then
      Result := clWhite
    else
      Result := Bmp.Canvas.Pixels[0, Bmp.Height - 1];
  end
  else
    Result := ColorToRGB(FTransparentColor);
  Result := Result or $02000000;
end;

procedure TJvBackgroundImage.PictureChanged(Sender: TObject);
begin
  if FInUpdWorkingBmp then
    Exit;
  FPictureValid := (FPicture.Width > 0) and (FPicture.Height > 0);
  if (FTileWidth < Picture.Width) or (FTileHeight < Picture.Height) or (AutoSizeTile and FPictureValid) then
  begin
    FTileWidth := Picture.Width;
    FTileHeight := Picture.Height;
  end;
  with Picture do
    if Graphic <> nil then
      Graphic.Transparent := FTransparent;
  UpdateWorkingBmp;
end;

procedure TJvBackgroundImage.SetAutoSizeTile(Value: Boolean);
begin
  if FAutoSizeTile <> Value then
  begin
    FAutoSizeTile := Value;
    if Mode = bmTile then
      if (TileWidth <> Picture.Width) or (TileHeight <> Picture.Height) then
        PictureChanged(Self);
  end;
end;

procedure TJvBackgroundImage.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TJvBackgroundImage.SetFitPictureSize(Value: Boolean);
begin
  if FFitPictureSize <> Value then
  begin
    FFitPictureSize := Value;
    if not (FMode in [bmTile, bmStretch]) then
      Changed;
  end;
end;

procedure TJvBackgroundImage.SetMode(Value: TJvBackgroundMode);
var
  TileModeChanged: Boolean;
begin
  if Value <> FMode then
  begin
    TileModeChanged := (FMode = bmTile) or (Value = bmTile);
    FMode := Value;
    if TileModeChanged and ((FTileWidth <> Picture.Width) or (FTileHeight <> Picture.Height)) then
      PictureChanged(Self)
    else
      Changed;
  end;
end;

procedure TJvBackgroundImage.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TJvBackgroundImage.SetShift(Value: Integer);
begin
  if Value <> FShift then
  begin
    FShift := Value;
    FZigZag := False;
    if FMode = bmTile then
      Changed;
  end;
end;

procedure TJvBackgroundImage.SetShiftMode(Value: TJvBackgroundShiftMode);
begin
  if FShiftMode <> Value then
  begin
    FShiftMode := Value;
    if FMode = bmTile then
      Changed;
  end;
end;

procedure TJvBackgroundImage.SetTileWidth(Value: Integer);
begin
  if AutoSizeTile then
    Exit;
  if Value < Picture.Width then
    Value := Picture.Width;
  if Value <> FTileWidth then
  begin
    FTileWidth := Value;
    if Mode = bmTile then
      PictureChanged(Self);
  end;
end;

procedure TJvBackgroundImage.SetTileHeight(Value: Integer);
begin
  if AutoSizeTile then
    Exit;
  if Value < Picture.Height then
    Value := Picture.Height;
  if Value <> FTileHeight then
  begin
    FTileHeight := Value;
    if Mode = bmTile then
      PictureChanged(Self);
  end;
end;

procedure TJvBackgroundImage.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    PictureChanged(Self);
  end;
end;

procedure TJvBackgroundImage.SetTransparentColor(Value: TColor);
begin
  if Value <> FTransparentColor then
  begin
    if Value = clDefault then
      FTransparentMode := tmAuto
    else
      FTransparentMode := tmFixed;
    FTransparentColor := Value;
    if Transparent then
      UpdateWorkingBmp;
  end;
end;

procedure TJvBackgroundImage.SetTransparentMode(Value: TTransparentMode);
begin
  if Value <> FTransparentMode then
  begin
    if Value = tmAuto then
      SetTransparentColor(clDefault)
    else
      SetTransparentColor(GetTransparentColor);
  end;
end;

procedure TJvBackgroundImage.SetZigZag(Value: Boolean);
begin
  if Value <> FZigZag then
  begin
    FZigZag := Value;
    if FMode = bmTile then
      Changed;
  end;
end;

function TJvBackgroundImage.TransparentColorStored: Boolean;
begin
  Result := FTransparentMode = tmFixed;
end;

{
  TJvBackgroundImage.UpdateWorkingBmp
  Transparency: all except TJPEGImage
  GrayMapping: all except TIcon, TMetafile
}

procedure TJvBackgroundImage.UpdateWorkingBmp;
var
  X, Y: Integer;
  IsBitmap: Boolean;
  Bmp: TBitmap;
  MaskBmp: TBitmap;
  {$IFNDEF NO_JPEG}
  GrayscaleState: Boolean;
  {$ENDIF !NO_JPEG}
  {$IFNDEF NO_JPEG}
  IsJPEG: Boolean;
  {$ENDIF !NO_JPEG}
  IsTransparent: Boolean;
  IsTranspGraphic: Boolean;
  IsIcon: Boolean;
  SizeTailored: Boolean;

  procedure DrawGraphic(Graphic: TGraphic);
  begin
    with FWorkingBmp.Canvas do
    begin
      Brush.Color := TransparentColor;
      FillRect(Rect(0, 0, FTileWidth, FTileHeight));
      Draw(X, Y, Graphic);
    end;
  end;

  function CreateTransparentBmp(Graphic: TGraphic): TBitmap;
  var
    W, H: Integer;
  begin
    Result := TBitmap.Create;
    if IsBitmap then
      Result.Assign(Graphic)
    else
    begin
      W := Graphic.Width;
      H := Graphic.Height;
      Result.Width := W;
      Result.Height := H;
      with Result.Canvas do
      begin
        Brush.Color := TransparentColor;
        FillRect(Rect(0, 0, W, H));
        Draw(0, 0, Graphic);
      end;
    end;
  end;

begin
  if FInUpdWorkingBmp then
    Exit;
  with FPicture do
    if Graphic <> nil then
    try
      FInUpdWorkingBmp := True;
      SizeTailored := False;
      X := 0;
      Y := 0;
      if FMode = bmTile then
      begin
        X := FTileWidth - Graphic.Width;
        Y := FTileHeight - Graphic.Height;
        SizeTailored := (X <> 0) or (Y <> 0);
        X := X div 2;
        Y := Y div 2;
      end;
      IsBitmap := (Graphic is TBitmap)
        // GIF goes as bitmap here
        {$IFDEF RECOGNIZE_GIF} or (Graphic is TGIFImage) {$ENDIF};
      IsIcon := Graphic is TIcon;
      IsTranspGraphic := IsIcon or (Graphic is TMetafile);
      // if Graphic is transparent
      {$IFDEF NO_JPEG}
      IsTransparent := Transparent or IsTranspGraphic;
      {$ELSE}
      IsJPEG := Graphic is TJPEGImage;
      IsTransparent := (Transparent and not IsJPEG) or IsTranspGraphic;
      {$ENDIF NO_JPEG}
      if IsTransparent or FGrayMapped or SizeTailored then
      begin
        WorkingBmpNeeded;
        if IsTranspGraphic then
          with FWorkingBmp.Canvas do
          begin
            Brush.Color := TransparentColor;
            FillRect(Rect(0, 0, FTileWidth, FTileHeight));
            Draw(X, Y, Graphic);
          end
        else
        if IsTransparent then // and not IsTranspGraphic
        begin
          Bmp := CreateTransparentBmp(Graphic);
          try
            with TImageList.CreateSize(Graphic.Width, Graphic.Height) do
            try
              if FGrayMapped then
              begin
                MaskBmp := TBitmap.Create;
                with MaskBmp do
                try
                  Assign(Bmp);
                  Mask(GetTransparentColor);
                  MapGrays(Bmp, FPicture.Graphic);
                  Add(Bmp, MaskBmp);
                finally
                  Free;
                end;
              end
              else
                AddMasked(Bmp, GetTransparentColor);
              FWorkingBmp.HandleType := bmDDB; // otherwise eventually background color won't appear correctly
              with FWorkingBmp.Canvas do
              begin
                Brush.Color := TransparentColor;
                FillRect(Rect(0, 0, FTileWidth, FTileHeight));
              end;
              BkColor := ColorToRGB(TransparentColor);
              Draw(FWorkingBmp.Canvas, X, Y, 0);
            finally
              Free;
            end
          finally
            Bmp.Free;
          end
        end
        else
        if GrayMapped then // and not Transparent
        begin
          Bmp := TBitmap.Create;
          try
            {$IFNDEF NO_JPEG}
            if IsJPEG then
              with TJPEGImage(Graphic) do
              begin
                GrayscaleState := Grayscale;
                try
                  Grayscale := True;
                  Bmp.Assign(Graphic);
                finally
                  Grayscale := GrayscaleState;
                end;
              end;
            {$ENDIF !NO_JPEG}
            MapGrays(Bmp, FPicture.Graphic);
            DrawGraphic(Bmp);
          finally
            Bmp.Free;
          end
        end
        else // if SizeTailored
          DrawGraphic(Picture.Graphic);
        WorkingBmp.Transparent := Transparent;
        WorkingBmp.TransparentColor := TransparentColor;
        Changed;
        Exit;
      end;
    finally
      FInUpdWorkingBmp := False;
    end;
  FWorkingBmp.Free;
  FWorkingBmp := nil;
  Changed;
end;

procedure TJvBackgroundImage.WorkingBmpNeeded;
var
  W, H: Integer;
begin
  if FWorkingBmp = nil then
    FWorkingBmp := TBitmap.Create;
  if FMode = bmTile then
  begin
    W := FTileWidth;
    H := FTileHeight;
  end
  else
  begin
    W := FPicture.Graphic.Width;
    H := FPicture.Graphic.Height;
  end;
  FWorkingBmp.Width := W;
  FWorkingBmp.Height := H;
end;

class function TJvBackgroundImage.MainWindowHook(var Msg: TMessage): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Msg.Msg = WM_SYSCOLORCHANGE then
  begin
    UpdateSysColorGradation;
    for I := 0 to Hooked.Count - 1 do
      TJvBackgroundImage(Hooked[I]).SysColorChange;
  end;
end;

procedure TJvBackgroundImage.HookMainWindow;
begin
  if Hooked = nil then
  begin
    Hooked := TList.Create;
    AddFinalizeObjectNil(sUnitName, TObject(Hooked));
    Application.HookMainWindow(MainWindowHook);
  end;
  if Hooked.IndexOf(Self) = -1 then
    Hooked.Add(Self);
end;

procedure TJvBackgroundImage.UnhookMainWindow;
begin
  Hooked.Remove(Self);
  if Hooked.Count = 0 then
  begin
    Application.UnhookMainWindow(MainWindowHook);
    Hooked.Free;
    Hooked := nil;
  end;
end;

procedure TJvBackgroundImage.SysColorChange;
begin
  if FGrayMapped then
    UpdateWorkingBmp;
end;

procedure TJvBackgroundImage.SetGrayMapped(Value: Boolean);
begin
  if Value <> FGrayMapped then
  begin
    if Value then
      SysColorsNeeded;
    FGrayMapped := Value;
    UpdateWorkingBmp;
  end;
end;

//=== { TJvControlBackground } ===============================================

constructor TJvControlBackground.Create(AClient: TWinControl);
begin
  inherited Create;
  FClient := AClient;
end;

function TJvControlBackground.HookBeforeMessage(var Msg: TMessage): Boolean;
begin
  Result := False;
  if FEnabled then
    case Msg.Msg of
      WM_PAINT:
        Result := HandleWMPaint(FClient, Msg);
      WM_ERASEBKGND:
        Result := HandleWMEraseBkgnd(FClient, Msg);
    end;
end;

procedure TJvControlBackground.HookAfterMessage(var Msg: TMessage);
begin
  if FEnabled then
    case Msg.Msg of
      WM_SIZE:
        if not (FMode in [bmTile, bmTopLeft]) then
          FClient.Invalidate;
      WM_HSCROLL:
        if FMode <> bmTile then
          FClient.Invalidate;
      WM_VSCROLL:
        if FMode <> bmTile then
          FClient.Invalidate;
    end;
end;

//=== { TJvBackgroundClientLink } ============================================

constructor TJvBackgroundClientLink.Create(ABackground: TJvBackground;
  AClient: TWinControl);
begin
  inherited Create;
  FBackground := ABackground;
  FNewWndProc := MakeObjectInstance(MainWndProc);
  ForceClient(AClient);
  ClientInvalidate;
end;

destructor TJvBackgroundClientLink.Destroy;
begin
  UnhookClient;
  if Assigned(FNewWndProc) then
    FreeObjectInstance(FNewWndProc);
  inherited Destroy;
end;

procedure TJvBackgroundClientLink.ClientInvalidate;
begin
  if not (csReading in FBackground.ComponentState) and not (csDestroying in FClient.ComponentState) then
    InvalidateRect(ClientHandle, nil, True);
end;

function GetMDIClientScrollDelta(ClientHandle: HWND; ScrollBar: Integer;
  const Msg: TWMScroll): Integer;
var
  ScrollInfo: TScrollInfo;
  Delta, MaxChange: Integer;
begin
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  GetScrollInfo(ClientHandle, ScrollBar, ScrollInfo);
  Delta := 0;
  case Msg.ScrollCode of
    SB_LINELEFT:
      begin
        Delta := ScrollInfo.nPos - ScrollInfo.nMin;
        if Delta > ScrollLineSize then
          Delta := ScrollLineSize;
      end;
    SB_LINERIGHT:
      with ScrollInfo do
      begin
        Delta := nPage - 1;
        if Delta < 0 then
          Delta := 0;
        Delta := nPos - (nMax - Delta);
        if Delta < -ScrollLineSize then
          Delta := -ScrollLineSize;
      end;
    SB_PAGELEFT:
      with ScrollInfo do
      begin
        Delta := nPage - 1;
        if Delta < 0 then
          Delta := 0;
        if Delta > nPos - nMin then
          Delta := nPos - nMin;
      end;
    SB_PAGERIGHT:
      with ScrollInfo do
      begin
        Delta := nPage - 1;
        if Delta < 0 then
          Delta := 0;
        MaxChange := (nMax - Delta) - nPos;
        if Delta > MaxChange then
          Delta := MaxChange;
        Delta := -Delta;
      end;
    SB_THUMBPOSITION:
      Delta := -Msg.Pos;
  end;
  Result := Delta * ScrollUnit;
end;

procedure TJvBackgroundClientLink.ClientWndProc(var Message: TMessage);

  procedure InvalidateBackground;
  begin
    InvalidateRect(ClientHandle, nil, True);
  end;

begin
  if ClientHandle <> 0 then
    with FBackground.FImage, Message do
    begin
      if FClientIsMDIForm then
      begin
        if Msg = WM_ERASEBKGND then
          if FEnabled and PaintBackground(FClient, TWMEraseBkgnd(Message).DC) then
          begin
            Result := 1;
            Exit;
          end;
      end
      else // not FClientIsMDIForm
      begin
        if FEnabled then
          case Msg of
            WM_PAINT:
              if HandleWMPaint(FClient, Message) then
                Exit;
            WM_ERASEBKGND:
              if HandleWMEraseBkgnd(FClient, Message) then
                Exit;
          end;
        Result := CallWindowProc(FPrevWndProc, ClientHandle, Msg, wParam, lParam);
        if Msg = CM_RELEASE then
          Exit;
      end;
      case Msg of
        WM_DESTROY:
          begin
            UnhookClient;
            if not (csDestroying in FClient.ComponentState) then
              PostMessage(FBackground.FHandle, CM_RECREATEWINDOW, 0, Longint(Self));
          end;
        WM_SIZE:
          if not (FMode in [bmTile, bmTopLeft]) then
            InvalidateBackground;
        WM_HSCROLL:
          begin
            if FClientIsMDIForm then
              Inc(FHorzOffset, GetMDIClientScrollDelta(ClientHandle,
                SB_HORZ, TWMScroll(Message)));
            if FMode <> bmTile then
              InvalidateBackground;
          end;
        WM_VSCROLL:
          begin
            if FClientIsMDIForm then
              Inc(FVertOffset, GetMDIClientScrollDelta(ClientHandle,
                SB_VERT, TWMScroll(Message)));
            if FMode <> bmTile then
              InvalidateBackground;
          end;
      end;
      if FClientIsMDIForm then
        Result := CallWindowProc(FPrevWndProc, ClientHandle, Msg, wParam, lParam);
    end;
end;

procedure TJvBackgroundClientLink.MainWndProc(var Msg: TMessage);
begin
  try
    try
      ClientWndProc(Msg);
    finally
      //FreeDeviceContexts;
      FreeMemoryContexts;
    end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TJvBackgroundClientLink.ForceClient(Value: TWinControl; Force: Boolean = True);
var
  I: Integer;
  Bk: TJvBackground;
begin
  if Value <> FClient then
  begin
    for I := 0 to Backgrounds.Count - 1 do
    begin
      Bk := Backgrounds[I];
      if (Bk <> FBackground) and Bk.HasClient(Value) then
        if Force then
        begin
          Bk.Clients.Remove(Value);
          Break;
        end
        else
          Exit;
    end;
    UnhookClient;
    {$IFDEF COMPILER5_UP}
    if Assigned(FClient) then
      FBackground.RemoveFreeNotification(FClient);
    {$ENDIF COMPILER5_UP}
    FClient := Value;
    if Assigned(Value) then
    begin
      FClientIsMDIForm := IsMDIForm(Value);
      FBackground.FreeNotification(Value);
      if not (csLoading in FBackground.ComponentState) then
        HookClient;
    end;
  end;
end;

procedure TJvBackgroundClientLink.HookClient;
begin
  {$IFDEF NO_DESIGNHOOK}
  if csDesigning in ComponentState then
    Exit;
  {$ENDIF NO_DESIGNHOOK}
  if Assigned(FClient) and not Assigned(FPrevWndProc) then
    if not ((csLoading in FClient.ComponentState) or ((FClient is TCustomForm) and (csDesigning in FClient.ComponentState))) then
    begin
      FClient.HandleNeeded;
      FPrevWndProc := Pointer(SetWindowLong(ClientHandle, GWL_WNDPROC, Longint(FNewWndProc)));
      FBackground.FImage.UpdateWorkingBmp;
    end;
end;

procedure TJvBackgroundClientLink.UnhookClient;
const
  WorkaroundStr: array [Boolean] of string = ('', SWorkaround);
begin
  if Assigned(FPrevWndProc) then
    if Assigned(FClient) then
    begin
      if FClient.HandleAllocated then
      begin
        if (Longint(FNewWndProc) <>
          SetWindowLong(ClientHandle, GWL_WNDPROC, Longint(FPrevWndProc))) and
          not (csDestroying in FClient.ComponentState) then
          MessageDlg(Format(SChainError, [FBackground.Owner.Name, FBackground.Name, FClient.Name,
            WorkaroundStr[csDesigning in FBackground.ComponentState]]),
              mtWarning, [mbOK], 0);
      end;
      FPrevWndProc := nil;
      ClientInvalidate;
      FClientIsMDIForm := False;
    end;
end;

function TJvBackgroundClientLink.GetClientColor: TColor;
begin
  Result := TWinControlAccessProtected(FClient).Color;
end;

function TJvBackgroundClientLink.GetClientHandle: HWND;
begin
  Result := JvBackgrounds.GetClientHandle(FClient);
  {
  Result := 0;
  if FClientIsMDIForm then
    Result := TForm(FClient).ClientHandle
  else
  if FClient.HandleAllocated then
    Result := FClient.Handle;
  }
end;

procedure TJvBackgroundClientLink.SetClient(Value: TWinControl);
begin
  ForceClient(Value);
end;

procedure TJvBackgroundClientLink.Release;
begin
  UnhookClient;
  PostMessage(FBackground.FHandle, CM_RELEASECLIENTLINK, 0, Longint(Self));
end;

//=== { TJvBackgroundClients } ===============================================

constructor TJvBackgroundClients.Create(ABackground: TJvBackground);
begin
  inherited Create;
  FBackground := ABackground;
  FLinks := TObjectList.Create;
  FLinks.OwnsObjects := False;
end;

destructor TJvBackgroundClients.Destroy;
begin
  FLinks.Clear;
  FLinks.Free;
  inherited Destroy;
end;

procedure TJvBackgroundClients.Clear;
var
  I: Integer;
begin
  for I := 0 to FLinks.Count - 1 do
    Links[I].Release;
  FLinks.Clear;
end;

procedure TJvBackgroundClients.Add(Control: TWinControl);
begin
  if IndexOf(Control) < 0 then
    FLinks.Add(TJvBackgroundClientLink.Create(FBackground, Control));
end;

procedure TJvBackgroundClients.Remove(Control: TWinControl);
var
  I: Integer;
  Link: TJvBackgroundClientLink;
begin
  I := IndexOf(Control);
  if I >= 0 then
  begin
    Link := TJvBackgroundClientLink(Links[I]);
    FLinks.Delete(I);
    Link.Release;
  end;
end;

function TJvBackgroundClients.GetClient(Index: Integer): TWinControl;
begin
  Result := TJvBackgroundClientLink(FLinks[Index]).Client;
end;

function TJvBackgroundClients.IndexOf(Control: TWinControl): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FLinks.Count - 1 do
    if Links[I].Client = Control then
    begin
      Result := I;
      Exit;
    end;
end;

procedure TJvBackgroundClients.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
  Client: TWinControl;
begin
  // (rom) no inherited call?
  if Operation = opRemove then
    for I := 0 to FLinks.Count - 1 do
    begin
      Client := Links[I].Client;
      if AComponent = Client then
        Remove(Client);
    end;
end;

procedure TJvBackgroundClients.DefineProperties(Filer: TFiler);

  function WriteClients: Boolean;
  var
    I: Integer;
    AncestorClients: TJvBackgroundClients;
  begin
    AncestorClients := TJvBackgroundClients(Filer.Ancestor);
    if AncestorClients = nil then
      Result := True // FLinks.Count > 0
    else
    if AncestorClients.FLinks.Count <> FLinks.Count then
      Result := True
    else
    begin
      Result := False;
      for I := 0 to FLinks.Count - 1 do
      begin
        Result := not (Clients[I] = AncestorClients[I]);
        if Result then
          Break;
      end
    end;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Clients', ReadData, WriteData, WriteClients);
end;

procedure TJvBackgroundClients.ReadData(Reader: TReader);
begin
  FLinks.Free;
  FLinks := nil;
  // (rom) TObjectList and TStringList are incompatible. This is probably a BUG!
  TStringList(FLinks) := TStringList.Create;
  Reader.ReadListBegin;
  while not Reader.EndOfList do
    TStringList(FLinks).Add(Reader.ReadString);
  Reader.ReadListEnd;
end;

procedure TJvBackgroundClients.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to FLinks.Count - 1 do
    Writer.WriteString(Clients[I].Name);
  Writer.WriteListEnd;
end;

procedure TJvBackgroundClients.FixupReferences(Root: TComponent);
var
  I: Integer;
  S: string;
  ItemNames: TStringList;
  NextItem: TComponent;
begin
  ItemNames := TStringList(FLinks);
  FLinks := nil;
  with ItemNames do
  try
    FLinks := TObjectList.Create;
    FLinks.OwnsObjects := False;
    FLinks.Capacity := Capacity;
    for I := 0 to Count - 1 do
    begin
      S := Strings[I];
      if Root.Name = S then
        NextItem := Root
      else
        NextItem := Root.FindComponent(Strings[I]);
      if NextItem = nil then
        Break;
      if NextItem is TWinControl then
        Self.Add(TWinControl(NextItem));
    end;
  finally
    Free;
  end;
end;

function TJvBackgroundClients.GetLink(Index: Integer): TJvBackgroundClientLink;
begin
  Result := TJvBackgroundClientLink(FLinks[Index]);
end;

procedure TJvBackgroundClients.Invalidate;
var
  I: Integer;
begin
  for I := 0 to FLinks.Count - 1 do
    Links[I].ClientInvalidate;
end;

//=== { TJvBackground } ======================================================

var
  Registered: Boolean = False;

constructor TJvBackground.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := AllocateHWnd(WndProc);
  FImage := TJvBackgroundImage.Create;
  FImage.FOnChange := WallpaperChanged;
  if Backgrounds = nil then
  begin
    Backgrounds := TList.Create;
    AddFinalizeObjectNil(sUnitName, TObject(Backgrounds));
  end;
  Backgrounds.Add(Self);
  FClients := TJvBackgroundClients.Create(Self);
  if csDesigning in ComponentState then
    if Assigned(Owner) then
      if Owner is TWinControl then
        FClients.Add(TWinControl(Owner));
  if not Registered then
  begin
    Classes.RegisterClasses([TJvBackgroundImage]);
    Registered := True;
  end;
end;

destructor TJvBackground.Destroy;
begin
  DeallocateHWnd(FHandle);
  FClients.Free;
  Backgrounds.Remove(Self);
  FImage.Free;
  inherited Destroy;
end;

procedure TJvBackground.Loaded;
begin
  inherited Loaded;
  FClients.FixupReferences(Owner);
end;

procedure TJvBackground.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if not (csDestroying in ComponentState) and Assigned(FClients) then
    FClients.Notification(AComponent, Operation);
  inherited Notification(AComponent, Operation);
end;

procedure TJvBackground.SetClients(Value: TJvBackgroundClients);
begin
  // dummy method to make Clients property visible in Object Inspector
end;

procedure TJvBackground.WallpaperChanged;
begin
  Clients.Invalidate;
end;

procedure TJvBackground.WndProc(var Msg: TMessage);
begin
  try
    case Msg.Msg of
      CM_RECREATEWINDOW:
        TJvBackgroundClientLink(Msg.lParam).HookClient;
      CM_RELEASECLIENTLINK:
        TJvBackgroundClientLink(Msg.lParam).Free;
    else
      Msg.Result := DefWindowProc(FHandle, Msg.Msg, Msg.wParam, Msg.lParam);
    end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TJvBackground.SetImage(const Value: TJvBackgroundImage);
begin
  FImage.Assign(Value);
end;

function TJvBackground.HasClient(Control: TWinControl): Boolean;
begin
  Result := Clients.IndexOf(Control) >= 0;
end;

initialization

finalization
  FinalizeUnit(sUnitName);

end.

