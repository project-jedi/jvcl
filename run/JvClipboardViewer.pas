{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvClipView.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvClipboardViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Clipbrd, Forms,
  StdCtrls, ExtCtrls, Menus, Grids, JvExControls, JvExForms, JvExGrids;

type
  // To lift the ambiguity in BCB between TBitmap and Windows::TBitmap
  {$IFDEF BCB5} // (ahuser) BCB 6 has problems with this. For BCB 5 I don't know it.
  {$HPPEMIT '#define TBitmap Graphics::TBitmap'}
  {$ENDIF BCB5}

  TClipboardViewFormat = (cvDefault, cvEmpty, cvUnknown, cvText, cvBitmap,
    cvMetafile, cvPalette, cvOemText, cvPicture, cvComponent, cvIcon);
  TJvOnImageEvent = procedure(Sender: TObject; Image: TBitmap) of object;
  TJvOnTextEvent = procedure(Sender: TObject; AText: string) of object;

  TJvCustomClipboardViewer = class(TJvExScrollBox)
  private
    FWndNext: HWND;
    FChained: Boolean;
    FPaintControl: TComponent;
    FViewFormat: TClipboardViewFormat;
    FOnChange: TNotifyEvent;
    FOnImage: TJvOnImageEvent;
    FOnText: TJvOnTextEvent;
    function IsEmptyClipboard: Boolean;
    procedure ForwardMessage(var Msg: TMessage);
    procedure WMDestroyClipboard(var Msg: TMessage); message WM_DESTROYCLIPBOARD;
    procedure WMChangeCBChain(var Msg: TWMChangeCBChain); message WM_CHANGECBCHAIN;
    procedure WMDrawClipboard(var Msg: TMessage); message WM_DRAWCLIPBOARD;
    procedure WMNCDestroy(var Msg: TWMNCDestroy); message WM_NCDESTROY;
    procedure SetViewFormat(Value: TClipboardViewFormat);
    function GetClipboardFormatNames(Index: Integer): string;
  protected
    procedure Loaded; override;
    procedure Resize; override;
    procedure CreateWnd; override;
    procedure DestroyWindowHandle; override;
    procedure DoImage(Image:TBitmap);dynamic;
    procedure DoText(const AText:string);dynamic;
    procedure Change; dynamic;
    procedure CreatePaintControl; virtual;
    function GetDrawFormat: TClipboardViewFormat; virtual;
    function ValidFormat(Format: TClipboardViewFormat): Boolean; dynamic;
    property ViewFormat: TClipboardViewFormat read FViewFormat write
      SetViewFormat stored False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnImage:TJvOnImageEvent read FOnImage write FOnImage;
    property OnText:TJvOnTextEvent read FOnText write FOnText;
  public
    constructor Create(AOwner: TComponent); override;
    class function CanDrawFormat(ClipboardFormat: Word): Boolean;
    procedure EmptyClipboard;
    property ClipboardFormatNames[Index: Integer]: string read GetClipboardFormatNames;
  end;

  TJvClipboardViewer = class(TJvCustomClipboardViewer)
  published
    property Anchors;
    property BiDiMode;
    property Color default clWindow;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ParentColor default False;
    property ViewFormat;
    property OnImage;
    property OnText;
    property OnChange;
    property OnContextPopup;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

function ClipboardFormatToView(Value: Word): TClipboardViewFormat;

implementation

uses
  Math,
  JvJVCLUtils, JvJCLUtils, JvResources;

{ Utility routines }

function ClipboardFormatName(Format: Word): string;
var
  Buffer: array [0..255] of Char;
begin
  SetString(Result, Buffer, GetClipboardFormatName(Format, Buffer, 255));
  if Result = '' then
    case Format of
      CF_TEXT:
        Result := 'Text';
      CF_BITMAP:
        Result := 'Bitmap';
      CF_METAFILEPICT:
        Result := 'Metafile Picture';
      CF_SYLK:
        Result := 'SYLK';
      CF_DIF:
        Result := 'DIF';
      CF_TIFF:
        Result := 'Tag Image';
      CF_OEMTEXT:
        Result := 'OEM Text';
      CF_DIB:
        Result := 'DIB Bitmap';
      CF_PALETTE:
        Result := 'Palette';
      CF_PENDATA:
        Result := 'Pen Data';
      CF_RIFF:
        Result := 'RIFF File';
      CF_WAVE:
        Result := 'Wave';
      // (rom) check for problems before uncomment
      //CF_UNICODETEXT:
      //  Result := 'Unicode text';
      CF_ENHMETAFILE:
        Result := 'Enchanced Metafile';
      //CF_HDROP:
      //  Result := 'Drop files';
      //CF_LOCALE:
      //  Result := 'Locale data';
    end;
end;

function ViewToClipboardFormat(Value: TClipboardViewFormat): Word;
begin
  case Value of
    cvDefault, cvUnknown, cvEmpty:
      Result := 0;
    cvText:
      Result := CF_TEXT;
    cvBitmap:
      Result := CF_BITMAP;
    cvMetafile:
      Result := CF_METAFILEPICT;
    cvPalette:
      Result := CF_PALETTE;
    cvOemText:
      Result := CF_OEMTEXT;
    cvPicture:
      Result := CF_PICTURE; // CF_BITMAP, CF_METAFILEPICT
    cvComponent:
      Result := CF_COMPONENT; // CF_TEXT
    cvIcon:
      Result := CF_ICON; // CF_BITMAP
  else
    Result := 0;
  end;
end;

function ClipboardFormatToView(Value: Word): TClipboardViewFormat;
begin
  case Value of
    CF_TEXT:
      Result := cvText;
    CF_BITMAP:
      Result := cvBitmap;
    CF_METAFILEPICT:
      Result := cvMetafile;
    CF_ENHMETAFILE:
      Result := cvMetafile;
    CF_PALETTE:
      Result := cvPalette;
    CF_OEMTEXT:
      Result := cvOemText;
  else
    Result := cvDefault;
  end;
  if Value = CF_ICON then
    Result := cvIcon // CF_BITMAP
  else
  if Value = CF_PICTURE then
    Result := cvPicture // CF_BITMAP, CF_METAFILEPICT
  else
  if Value = CF_COMPONENT then
    Result := cvComponent; // CF_TEXT
end;

procedure ComponentToStrings(Instance: TComponent; Text: TStrings);
var
  Mem, MemOut: TMemoryStream;
begin
  Text.BeginUpdate;
  Mem := TMemoryStream.Create;
  try
    Mem.WriteComponent(Instance);
    Mem.Position := 0;
    MemOut := TMemoryStream.Create;
    try
      ObjectBinaryToText(Mem, MemOut);
      MemOut.Position := 0;
      Text.LoadFromStream(MemOut);
    finally
      MemOut.Free;
    end;
  finally
    Mem.Free;
    Text.EndUpdate;
  end;
end;

//=== TJvPaletteGrid =========================================================

const
  NumPaletteEntries = 256;

type
  TJvPaletteGrid = class(TJvExDrawGrid)
  private
    FPaletteEntries: array [0..NumPaletteEntries - 1] of TPaletteEntry;
    FPalette: HPALETTE;
    FCount: Integer;
    FSizing: Boolean;
    procedure SetPalette(Value: HPALETTE);
    procedure UpdateSize;
    function CellColor(ACol, ARow: Longint): TColor;
    procedure DrawSquare(CellColor: TColor; CellRect: TRect; ShowSelector: Boolean);
  protected
    function GetPalette: HPALETTE; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Palette: HPALETTE read FPalette write SetPalette;
  end;

function CopyPalette(Palette: HPALETTE): HPALETTE;
var
  PaletteSize: Integer;
  LogSize: Integer;
  LogPalette: PLogPalette;
begin
  Result := 0;
  if Palette = 0 then
    Exit;
  GetObject(Palette, SizeOf(PaletteSize), @PaletteSize);
  LogSize := SizeOf(TLogPalette) + (PaletteSize - 1) * SizeOf(TPaletteEntry);
  GetMem(LogPalette, LogSize);
  try
    with LogPalette^ do
    begin
      palVersion := $0300;
      palNumEntries := PaletteSize;
      GetPaletteEntries(Palette, 0, PaletteSize, palPalEntry);
    end;
    Result := CreatePalette(LogPalette^);
  finally
    FreeMem(LogPalette, LogSize);
  end;
end;

constructor TJvPaletteGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DefaultColWidth := 20;
  DefaultRowHeight := 20;
  Options := [];
  GridLineWidth := 0;
  FixedCols := 0;
  FixedRows := 0;
  ColCount := 0;
  RowCount := 0;
  DefaultDrawing := False;
  ScrollBars := ssVertical;
end;

destructor TJvPaletteGrid.Destroy;
begin
  if FPalette <> 0 then
    DeleteObject(FPalette);
  inherited Destroy;
end;

procedure TJvPaletteGrid.UpdateSize;
var
  Rows: Integer;
begin
  if FSizing then
    Exit;
  FSizing := True;
  try
    ColCount := (ClientWidth - GetSystemMetrics(SM_CXVSCROLL)) div DefaultColWidth;
    Rows := FCount div ColCount;
    if FCount mod ColCount > 0 then
      Inc(Rows);
    RowCount := Max(1, Rows);
    ClientHeight := DefaultRowHeight * RowCount;
  finally
    FSizing := False;
  end;
end;

function TJvPaletteGrid.GetPalette: HPALETTE;
begin
  if FPalette <> 0 then
    Result := FPalette
  else
    Result := inherited GetPalette;
end;

procedure TJvPaletteGrid.SetPalette(Value: HPALETTE);
var
  I: Integer;
  ParentForm: TCustomForm;
begin
  if FPalette <> 0 then
    DeleteObject(FPalette);
  FPalette := CopyPalette(Value);
  FCount := Min(PaletteEntries(FPalette), NumPaletteEntries);
  GetPaletteEntries(FPalette, 0, FCount, FPaletteEntries);
  for I := FCount to NumPaletteEntries - 1 do
    FillChar(FPaletteEntries[I], SizeOf(TPaletteEntry), $80);
  UpdateSize;
  if Visible and (not (csLoading in ComponentState)) then
  begin
    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) and ParentForm.Active and
      Parentform.HandleAllocated then
      PostMessage(ParentForm.Handle, WM_QUERYNEWPALETTE, 0, 0);
  end;
end;

function TJvPaletteGrid.CellColor(ACol, ARow: Longint): TColor;
var
  PalIndex: Integer;
begin
  PalIndex := ACol + (ARow * ColCount);
  if PalIndex <= FCount - 1 then
    with FPaletteEntries[PalIndex] do
      Result := TColor(RGB(peRed, peGreen, peBlue))
  else
    Result := clNone;
end;

procedure TJvPaletteGrid.DrawSquare(CellColor: TColor; CellRect: TRect;
  ShowSelector: Boolean);
var
  SavePal: HPalette;
begin
  Canvas.Pen.Color := clBtnFace;
  with CellRect do
    Canvas.Rectangle(Left, Top, Right, Bottom);
  InflateRect(CellRect, -1, -1);
  Frame3D(Canvas, CellRect, clBtnShadow, clBtnHighlight, 2);
  SavePal := 0;
  if FPalette <> 0 then
  begin
    SavePal := SelectPalette(Canvas.Handle, FPalette, False);
    RealizePalette(Canvas.Handle);
  end;
  try
    Canvas.Brush.Color := CellColor;
    Canvas.Pen.Color := CellColor;
    with CellRect do
      Canvas.Rectangle(Left, Top, Right, Bottom);
  finally
    if FPalette <> 0 then
      SelectPalette(Canvas.Handle, SavePal, True);
  end;
  if ShowSelector then
  begin
    Canvas.Brush.Color := Self.Color;
    Canvas.Pen.Color := Self.Color;
    InflateRect(CellRect, -1, -1);
    Canvas.DrawFocusRect(CellRect);
  end;
end;

function TJvPaletteGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := ((ACol = 0) and (ARow = 0)) or (CellColor(ACol, ARow) <> clNone);
end;

procedure TJvPaletteGrid.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
var
  Color: TColor;
begin
  Color := CellColor(ACol, ARow);
  if Color <> clNone then
    DrawSquare(PaletteColor(Color), ARect, gdFocused in AState)
  else
  begin
    Canvas.Brush.Color := Self.Color;
    Canvas.FillRect(ARect);
  end;
end;

procedure TJvPaletteGrid.Resize;
begin
  inherited Resize;
  UpdateSize;
end;

//=== TJvCustomClipboardViewer ===============================================

constructor TJvCustomClipboardViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlState := ControlState + [csCreating];
  FWndNext := 0;
  FPaintControl := nil;
  FViewFormat := cvDefault;
  ParentColor := False;
  Color := clWindow;
  ControlState := ControlState - [csCreating];
end;

procedure TJvCustomClipboardViewer.ForwardMessage(var Msg: TMessage);
begin
  if FWndNext <> 0 then
    with Msg do
      SendMessage(FWndNext, Msg, WParam, LParam);
end;

procedure TJvCustomClipboardViewer.CreateWnd;
begin
  inherited CreateWnd;
  if Handle <> 0 then
  begin
    FWndNext := SetClipboardViewer(Handle);
    FChained := True;
  end;
end;

procedure TJvCustomClipboardViewer.DestroyWindowHandle;
begin
  if FChained then
  begin
    ChangeClipboardChain(Handle, FWndNext);
    FChained := False;
  end;
  FWndNext := 0;
  inherited DestroyWindowHandle;
end;

procedure TJvCustomClipboardViewer.CreatePaintControl;
var
  Icon: TIcon;
  Format: TClipboardViewFormat;
  Instance: TComponent;
begin
  if csDesigning in ComponentState then
    Exit;
  FPaintControl.Free;
  FPaintControl := nil;
  if IsEmptyClipboard then
    Exit;
  Format := GetDrawFormat;
  if not ValidFormat(Format) then
    Format := cvUnknown;
  case Format of
    cvText, cvOemText, cvUnknown, cvDefault, cvEmpty:
      begin
        FPaintControl := TMemo.Create(Self);
        with TMemo(FPaintControl) do
        begin
          BorderStyle := bsNone;
          Parent := Self;
          Left := 0;
          Top := 0;
          ScrollBars := ssBoth;
          Align := alClient;
          if Format = cvOemText then
          begin
            ParentFont := False;
            Font.Name := 'Terminal';
          end;
          Visible := True;
          if Clipboard.HasFormat(CF_TEXT) then
            PasteFromClipboard
          else
            if (Format = cvText) and Clipboard.HasFormat(CF_COMPONENT) then
          begin
            Instance := Clipboard.GetComponent(Self, Self);
            try
              ComponentToStrings(Instance, Lines);
            finally
              Instance.Free;
            end;
          end
          else
            if IsEmptyClipboard then
            Text := RsClipboardEmpty
          else
            Text := RsClipboardUnknown;
          ReadOnly := True;
        end;
      end;
    cvPicture, cvMetafile, cvBitmap, cvIcon:
      begin
        FPaintControl := TImage.Create(Self);
        with TImage(FPaintControl) do
        begin
          Parent := Self;
          AutoSize := True;
          Left := 0;
          Top := 0;
          Visible := True;
          if Format = cvIcon then
          begin
            if Clipboard.HasFormat(CF_ICON) then
            begin
              Icon := CreateIconFromClipboard;
              try
                Picture.Icon := Icon;
              finally
                Icon.Free;
              end;
            end;
          end
          else
            if ((Format = cvBitmap) and Clipboard.HasFormat(CF_BITMAP))
            or ((Format = cvMetafile) and (Clipboard.HasFormat(CF_METAFILEPICT))
            or Clipboard.HasFormat(CF_ENHMETAFILE))
            or ((Format = cvPicture) and Clipboard.HasFormat(CF_PICTURE)) then
          begin
            Picture.Assign(Clipboard);
          end;
        end;
        CenterControl(TImage(FPaintControl));
      end;
    cvComponent:
      begin
        Instance := Clipboard.GetComponent(Self, Self);
        FPaintControl := Instance;
        if FPaintControl is TControl then
        begin
          with TControl(FPaintControl) do
          begin
            Left := 1;
            Top := 1;
            Parent := Self;
          end;
          CenterControl(TControl(FPaintControl));
        end
        else
        begin
          FPaintControl := TMemo.Create(Self);
          try
            with TMemo(FPaintControl) do
            begin
              BorderStyle := bsNone;
              Parent := Self;
              Left := 0;
              Top := 0;
              ScrollBars := ssBoth;
              Align := alClient;
              ReadOnly := True;
              ComponentToStrings(Instance, Lines);
              Visible := True;
            end;
          finally
            Instance.Free;
          end;
        end;
      end;
    cvPalette:
      begin
        FPaintControl := TJvPaletteGrid.Create(Self);
        with TJvPaletteGrid(FPaintControl) do
        try
          BorderStyle := bsNone;
          Parent := Self;
          Ctl3D := False;
          Align := alClient;
          Clipboard.Open;
          try
            Palette := GetClipboardData(CF_PALETTE);
          finally
            Clipboard.Close;
          end;
        except
          FPaintControl.Free;
          raise;
        end;
      end;
  end;
end;

function TJvCustomClipboardViewer.GetClipboardFormatNames(Index: Integer): string;
begin
  Result := '';
  if Index < Clipboard.FormatCount then
    Result := ClipboardFormatName(Clipboard.Formats[Index]);
end;

procedure TJvCustomClipboardViewer.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvCustomClipboardViewer.Loaded;
begin
  inherited Loaded;
  Resize; // Resize is not called while csLoading in ComponentState
end;

procedure TJvCustomClipboardViewer.Resize;
begin
  inherited Resize;
  if (FPaintControl <> nil) and (FPaintControl is TControl) then
    CenterControl(TControl(FPaintControl));
end;

procedure TJvCustomClipboardViewer.WMChangeCBChain(var Msg: TWMChangeCBChain);
begin
  if Msg.Remove = FWndNext then
    FWndNext := Msg.Next
  else
    ForwardMessage(TMessage(Msg));
  inherited;
end;

procedure TJvCustomClipboardViewer.WMNCDestroy(var Msg: TWMNCDestroy);
begin
  if FChained then
  begin
    ChangeClipboardChain(Handle, FWndNext);
    FChained := False;
    FWndNext := 0;
  end;
  inherited;
end;

procedure TJvCustomClipboardViewer.WMDrawClipboard(var Msg: TMessage);
var
  Format: Word;
  B:TBitmap;
begin
  ForwardMessage(Msg);
  Format := ViewToClipboardFormat(ViewFormat);
  if IsEmptyClipboard then
    FViewFormat := cvEmpty
  else
  if not Clipboard.HasFormat(Format) then
    FViewFormat := cvDefault;
  if Clipboard.HasFormat(CF_BITMAP) then
  begin
    B := TBitmap.Create;
    try
      B.Assign(Clipboard);
      DoImage(B);
    finally
      B.Free;
    end;
  end;
  if Clipboard.HasFormat(CF_TEXT) then
    DoText(Clipboard.AsText);
  Change;
  DisableAlign;
  try
    CreatePaintControl;
  finally
    EnableAlign;
  end;
  inherited;
end;

procedure TJvCustomClipboardViewer.WMDestroyClipboard(var Msg: TMessage);
begin
  FViewFormat := cvEmpty;
  Change;
  CreatePaintControl;
end;

function TJvCustomClipboardViewer.IsEmptyClipboard: Boolean;
begin
  Result := (Clipboard.FormatCount = 0);
end;

procedure TJvCustomClipboardViewer.SetViewFormat(Value: TClipboardViewFormat);
var
  Format: Word;
begin
  if Value <> ViewFormat then
  begin
    Format := ViewToClipboardFormat(Value);
    if (Clipboard.HasFormat(Format) and ValidFormat(Value)) then
      FViewFormat := Value
    else
      FViewFormat := cvDefault;
    CreatePaintControl;
  end;
end;

function TJvCustomClipboardViewer.GetDrawFormat: TClipboardViewFormat;

  function DefaultFormat: TClipboardViewFormat;
  begin
    if Clipboard.HasFormat(CF_TEXT) then
      Result := cvText
    else
    if Clipboard.HasFormat(CF_OEMTEXT) then
      Result := cvOemText
    else
    if Clipboard.HasFormat(CF_BITMAP) then
      Result := cvBitmap
    else
    if Clipboard.HasFormat(CF_METAFILEPICT) then
      Result := cvMetafile
    else
    if Clipboard.HasFormat(CF_ENHMETAFILE) then
      Result := cvMetafile
    else
    if Clipboard.HasFormat(CF_ICON) then
      Result := cvIcon
    else
      if Clipboard.HasFormat(CF_PICTURE) then
      Result := cvPicture
    else
    if Clipboard.HasFormat(CF_COMPONENT) then
      Result := cvComponent
    else
    if Clipboard.HasFormat(CF_PALETTE) then
      Result := cvPalette
    else
      Result := cvUnknown;
  end;

begin
  if IsEmptyClipboard then
    Result := cvEmpty
  else
  begin
    Result := ViewFormat;
    if Result = cvDefault then
      Result := DefaultFormat;
  end;
end;

class function TJvCustomClipboardViewer.CanDrawFormat(ClipboardFormat: Word): Boolean;
begin
  Result := ClipboardFormatToView(ClipboardFormat) <> cvUnknown;
end;

function TJvCustomClipboardViewer.ValidFormat(Format: TClipboardViewFormat): Boolean;
begin
  Result := (Format in [cvDefault, cvEmpty, cvUnknown]);
  if not Result then
    if Clipboard.HasFormat(ViewToClipboardFormat(Format)) then
      Result := True;
end;

procedure TJvCustomClipboardViewer.DoImage(Image: TBitmap);
begin
  if Assigned(FOnImage) then
    FOnImage(self,Image);
end;

procedure TJvCustomClipboardViewer.DoText(const AText: string);
begin
  if Assigned(FOnText) then
    FOnText(self,AText);
end;

procedure TJvCustomClipboardViewer.EmptyClipboard;
begin
  OpenClipboard(Application.Handle);
  // (rom) added Windows. to avoid recursion
  Windows.EmptyClipboard;
  CloseClipboard;
end;

end.

