{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCombos.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JvCombos;

{.$DEFINE GXE}
{ Activate this define to use JvCombos in the GXExplorer Open Source project }


interface

uses {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Controls, Graphics, StdCtrls, Forms, Menus;

type

{ TJvOwnerDrawComboBox }

  TOwnerDrawComboStyle = csDropDown..csDropDownList;

  TJvOwnerDrawComboBox = class(TCustomComboBox)
  private
    FStyle: TOwnerDrawComboStyle;
    FItemHeightChanging: Boolean;
    procedure SetComboStyle(Value: TOwnerDrawComboStyle);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
{$IFDEF Delphi3_Up}
    procedure CMRecreateWnd(var Message: TMessage); message CM_RECREATEWND;
{$ENDIF}
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure ResetItemHeight;
    function MinItemHeight: Integer; virtual;
    property Style: TOwnerDrawComboStyle read FStyle write SetComboStyle
      default csDropDownList;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TJvxColorComboBox }

{$IFDEF Delphi3_Up}
  TColorComboOption = (coIncludeDefault, coIncludeNone);
  TColorComboOptions = set of TColorComboOption;
{$ENDIF}

  TJvxColorComboBox = class(TJvOwnerDrawComboBox)
  private
    FColorValue: TColor;
    FDisplayNames: Boolean;
    FColorNames: TStrings;
{$IFDEF Delphi3_Up}
    FOptions: TColorComboOptions;
{$ENDIF}
    FOnChange: TNotifyEvent;
    function GetColorValue: TColor;
    procedure SetColorValue(NewValue: TColor);
    procedure SetDisplayNames(Value: Boolean);
    procedure SetColorNames(Value: TStrings);
{$IFDEF Delphi3_Up}
    procedure SetOptions(Value: TColorComboOptions);
{$ENDIF}
    procedure ColorNamesChanged(Sender: TObject);
  protected
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure Click; override;
    procedure Change; override;
    procedure PopulateList; virtual;
    procedure DoChange; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Text;
  published
    property ColorValue: TColor read GetColorValue write SetColorValue
      default clBlack;
    property ColorNames: TStrings read FColorNames write SetColorNames;
    property DisplayNames: Boolean read FDisplayNames write SetDisplayNames
      default True;
{$IFDEF Delphi3_Up}
    property Options: TColorComboOptions read FOptions write SetOptions
      default [];
{$ENDIF}
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
{$IFDEF Delphi4_Up}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
{$IFDEF WIN32}
  {$IFNDEF VER90}
    property ImeMode;
    property ImeName;
  {$ENDIF}
{$ENDIF}
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
{$IFDEF Delphi5_Up}
    property OnContextPopup;
{$ENDIF}
{$IFDEF Delphi4_Up}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

{ TJvxFontComboBox }

  TFontDevice = (fdScreen, fdPrinter, fdBoth);
  TFontListOption = (foAnsiOnly, foTrueTypeOnly, foFixedPitchOnly,
    foNoOEMFonts, foOEMFontsOnly, foScalableOnly, foNoSymbolFonts);
  TFontListOptions = set of TFontListOption;

  TJvxFontComboBox = class(TJvOwnerDrawComboBox)
  private
    FTrueTypeBMP: TBitmap;
    FDeviceBMP: TBitmap;
    FOnChange: TNotifyEvent;
    FDevice: TFontDevice;
    FUpdate: Boolean;
    FUseFonts: Boolean;
    FOptions: TFontListOptions;
    procedure SetFontName(const NewFontName: TFontName);
    function GetFontName: TFontName;
    function GetTrueTypeOnly: Boolean;
    procedure SetDevice(Value: TFontDevice);
    procedure SetOptions(Value: TFontListOptions);
    procedure SetTrueTypeOnly(Value: Boolean);
    procedure SetUseFonts(Value: Boolean);
    procedure Reset;
    procedure WMFontChange(var Message: TMessage); message WM_FONTCHANGE;
  protected
    procedure PopulateList; virtual;
    procedure Change; override;
    procedure Click; override;
    procedure DoChange; dynamic;
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    function MinItemHeight: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Text;
  published
    property Device: TFontDevice read FDevice write SetDevice default fdScreen;
    property FontName: TFontName read GetFontName write SetFontName;
    property Options: TFontListOptions read FOptions write SetOptions default [];
    property TrueTypeOnly: Boolean read GetTrueTypeOnly write SetTrueTypeOnly
      stored False; { obsolete, use Options instead }
    property UseFonts: Boolean read FUseFonts write SetUseFonts default False;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
{$IFDEF Delphi4_Up}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
{$IFDEF WIN32}
  {$IFNDEF VER90}
    property ImeMode;
    property ImeName;
  {$ENDIF}
{$ENDIF}
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
{$IFDEF Delphi5_Up}
    property OnContextPopup;
{$ENDIF}
{$IFDEF Delphi4_Up}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

{$IFDEF GXE}
procedure Register;
{$ENDIF}

implementation

{$IFDEF WIN32}
 {$R *.Res}
{$ELSE}
 {$R *.R16}
{$ENDIF}

uses SysUtils, Consts, Printers {$IFNDEF GXE}, JvVCLUtils {$ENDIF};

{$IFDEF GXE}
procedure Register;
begin
  RegisterComponents('Additional', [TJvxFontComboBox, TJvxColorComboBox]);
end;
{$ENDIF GXE}

{$IFNDEF WIN32}
type
  DWORD = Longint;
{$ENDIF}

{ Utility routines }

function CreateBitmap(ResName: PChar): TBitmap;
begin
{$IFDEF GXE}
  Result := TBitmap.Create;
  Result.Handle := LoadBitmap(HInstance, ResName);
{$ELSE}
  Result := MakeModuleBitmap(HInstance, ResName);
  if Result = nil then ResourceNotFound(ResName);
{$ENDIF GXE}
end;

function GetItemHeight(Font: TFont): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
  Result := Metrics.tmHeight + 1;
end;

{ TJvOwnerDrawComboBox }

constructor TJvOwnerDrawComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited Style := csDropDownList;
  FStyle := csDropDownList;
end;

procedure TJvOwnerDrawComboBox.SetComboStyle(Value: TOwnerDrawComboStyle);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    inherited Style := Value;
  end;
end;

function TJvOwnerDrawComboBox.MinItemHeight: Integer;
begin
  Result := GetItemHeight(Font);
  if Result < 9 then Result := 9;
end;

procedure TJvOwnerDrawComboBox.ResetItemHeight;
var
  H: Integer;
begin
  H := MinItemHeight;
  FItemHeightChanging := True;
  try
    inherited ItemHeight := H;
  finally
    FItemHeightChanging := False;
  end;
  if HandleAllocated then SendMessage(Handle, CB_SETITEMHEIGHT, 0, H);
end;

procedure TJvOwnerDrawComboBox.CreateParams(var Params: TCreateParams);
const
  ComboBoxStyles: array[TOwnerDrawComboStyle] of DWORD =
    (CBS_DROPDOWN, CBS_SIMPLE, CBS_DROPDOWNLIST);
begin
  inherited CreateParams(Params);
  with Params do
    Style := (Style and not CBS_DROPDOWNLIST) or CBS_OWNERDRAWFIXED or
      ComboBoxStyles[FStyle];
end;

procedure TJvOwnerDrawComboBox.CreateWnd;
begin
  inherited CreateWnd;
  ResetItemHeight;
end;

procedure TJvOwnerDrawComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
  RecreateWnd;
end;

{$IFDEF Delphi3_Up}
procedure TJvOwnerDrawComboBox.CMRecreateWnd(var Message: TMessage);
begin
  if not FItemHeightChanging then
    inherited;
end;
{$ENDIF}

{ TJvxColorComboBox }

const
  ColorsInList = {$IFDEF Delphi3_Up} 18 {$ELSE} 16 {$ENDIF};
  ColorValues: array [0..ColorsInList - 1] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite
    {$IFDEF Delphi3_Up}, clNone, clDefault {$ENDIF});

constructor TJvxColorComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorValue := clBlack;  { make default color selected }
  FColorNames := TStringList.Create;
  TStringList(FColorNames).OnChange := ColorNamesChanged;
  FDisplayNames := True;
end;

destructor TJvxColorComboBox.Destroy;
begin
  TStringList(FColorNames).OnChange := nil;
  FColorNames.Free;
  FColorNames := nil;
  inherited Destroy;
end;

procedure TJvxColorComboBox.CreateWnd;
begin
  inherited CreateWnd;
  PopulateList;
  SetColorValue(FColorValue);
end;

procedure TJvxColorComboBox.PopulateList;
var
  I: Integer;
  ColorName: string;
begin
  Items.BeginUpdate;
  try
    Clear;
    for I := 0 to Pred(ColorsInList) do begin
{$IFDEF Delphi3_Up}
      if ((ColorValues[I] = clDefault) and not (coIncludeDefault in Options)) or
        ((ColorValues[I] = clNone) and not (coIncludeNone in Options)) then
        Continue;
{$ENDIF}
      if (I <= Pred(FColorNames.Count)) and (FColorNames[I] <> '') then
        ColorName := FColorNames[I]
{$IFDEF Delphi3_Up}
      else if ColorValues[I] = clDefault then ColorName := SDefault
{$ENDIF}
      else
        { delete two first characters which prefix "cl" educated }
        ColorName := Copy(ColorToString(ColorValues[I]), 3, MaxInt);
      Items.AddObject(ColorName, TObject(ColorValues[I]));
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TJvxColorComboBox.ColorNamesChanged(Sender: TObject);
begin
  if HandleAllocated then begin
    FColorValue := ColorValue;
    RecreateWnd;
  end;
end;

procedure TJvxColorComboBox.SetColorNames(Value: TStrings);
begin
  FColorNames.Assign(Value);
end;

procedure TJvxColorComboBox.SetDisplayNames(Value: Boolean);
begin
  if DisplayNames <> Value then begin
    FDisplayNames := Value;
    Invalidate;
  end;
end;

{$IFDEF Delphi3_Up}
procedure TJvxColorComboBox.SetOptions(Value: TColorComboOptions);
begin
  if FOptions <> Value then begin
    FOptions := Value;
    if HandleAllocated then RecreateWnd;
  end;
end;
{$ENDIF}

function TJvxColorComboBox.GetColorValue: TColor;
var
  I: Integer;
begin
  Result := FColorValue;
  if (Style <> csDropDownList) and (ItemIndex < 0) then begin
    I := Items.IndexOf(inherited Text);
    if I >= 0 then Result := TColor(Items.Objects[I])
    else begin
      Val(inherited Text, Result, I);
      if I <> 0 then Result := FColorValue;
    end;
  end;
end;

procedure TJvxColorComboBox.SetColorValue(NewValue: TColor);
var
  Item: Integer;
  CurrentColor: TColor;
  S: string;
begin
  if (ItemIndex < 0) or (NewValue <> FColorValue) then begin
    FColorValue := NewValue;
    { change selected item }
    for Item := 0 to Pred(Items.Count) do begin
      CurrentColor := TColor(Items.Objects[Item]);
      if CurrentColor = NewValue then begin
        if ItemIndex <> Item then ItemIndex := Item;
        DoChange;
        Exit;
      end;
    end;
    if Style = csDropDownList then
      ItemIndex := -1
    else begin
      S := ColorToString(NewValue);
      if Pos('cl', S) = 1 then System.Delete(S, 1, 2);
      inherited Text := S;
    end;
    DoChange;
  end;
end;

procedure TJvxColorComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);

  function ColorToBorderColor(AColor: TColor): TColor;
  type
    TColorQuad = record
      Red, Green, Blue, Alpha: Byte;
    end;
  begin
    if (TColorQuad(AColor).Red > 192) or (TColorQuad(AColor).Green > 192) or
       (TColorQuad(AColor).Blue > 192) then
      Result := clBlack
    else if (odSelected in State) then
      Result := clWhite
    else
      Result := AColor;
  end;

const
  ColorWidth = 22;
var
  ARect: TRect;
  Text: array[0..255] of Char;
  Safer: TColor;
begin
  ARect := Rect;
  Inc(ARect.Top, 2);
  Inc(ARect.Left, 2);
  Dec(ARect.Bottom, 2);
  if FDisplayNames then ARect.Right := ARect.Left + ColorWidth
  else Dec(ARect.Right, 3);
  with Canvas do begin
    FillRect(Rect);
    Safer := Brush.Color;
    Pen.Color := ColorToBorderColor(ColorToRGB(TColor(Items.Objects[Index])));
    Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    Brush.Color := TColor(Items.Objects[Index]);
    try
      InflateRect(ARect, -1, -1);
      FillRect(ARect);
    finally
      Brush.Color := Safer;
    end;
    if FDisplayNames then begin
      StrPCopy(Text, Items[Index]);
      Rect.Left := Rect.Left + ColorWidth + 6;
      DrawText(Canvas.Handle, Text, StrLen(Text), Rect,
{$IFDEF Delphi4_Up}
        DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX));
{$ELSE}
        DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
{$ENDIF}
    end;
  end;
end;

procedure TJvxColorComboBox.Change;
var
  AColor: TColor;
begin
  inherited Change;
  AColor := GetColorValue;
  if FColorValue <> AColor then begin
    FColorValue := AColor;
    DoChange;
  end;
end;

procedure TJvxColorComboBox.Click;
begin
  if ItemIndex >= 0 then ColorValue := TColor(Items.Objects[ItemIndex]);
  inherited Click;
end;

procedure TJvxColorComboBox.DoChange;
begin
  if not (csReading in ComponentState) then
    if Assigned(FOnChange) then FOnChange(Self);
end;

{ TJvxFontComboBox }

const
  WRITABLE_FONTTYPE = 256;

function IsValidFont(Box: TJvxFontComboBox; LogFont: TLogFont;
  FontType: Integer): Boolean;
begin
  Result := True;
  if (foAnsiOnly in Box.Options) then
    Result := Result and (LogFont.lfCharSet = ANSI_CHARSET);
  if (foTrueTypeOnly in Box.Options) then
    Result := Result and (FontType and TRUETYPE_FONTTYPE = TRUETYPE_FONTTYPE);
  if (foFixedPitchOnly in Box.Options) then
    Result := Result and (LogFont.lfPitchAndFamily and FIXED_PITCH = FIXED_PITCH);
  if (foOEMFontsOnly in Box.Options) then
    Result := Result and (LogFont.lfCharSet = OEM_CHARSET);
  if (foNoOEMFonts in Box.Options) then
    Result := Result and (LogFont.lfCharSet <> OEM_CHARSET);
  if (foNoSymbolFonts in Box.Options) then
    Result := Result and (LogFont.lfCharSet <> SYMBOL_CHARSET);
  if (foScalableOnly in Box.Options) then
    Result := Result and (FontType and RASTER_FONTTYPE = 0);
end;

{$IFDEF WIN32}

function EnumFontsProc(var EnumLogFont: TEnumLogFont;
  var TextMetric: TNewTextMetric; FontType: Integer; Data: LPARAM): Integer;
  export; stdcall;
var
  FaceName: string;
begin
  FaceName := StrPas(EnumLogFont.elfLogFont.lfFaceName);
  with TJvxFontComboBox(Data) do
    if (Items.IndexOf(FaceName) < 0) and
      IsValidFont(TJvxFontComboBox(Data), EnumLogFont.elfLogFont, FontType) then
    begin
      if EnumLogFont.elfLogFont.lfCharSet <> SYMBOL_CHARSET then
        FontType := FontType or WRITABLE_FONTTYPE;
      Items.AddObject(FaceName, TObject(FontType));
    end;
  Result := 1;
end;

{$ELSE}

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; export;
begin
  with TJvxFontComboBox(Data) do
    if (Items.IndexOf(StrPas(LogFont.lfFaceName)) < 0) and
      IsValidFont(TJvxFontComboBox(Data), LogFont, FontType) then
    begin
      if LogFont.lfCharSet = SYMBOL_CHARSET then
        FontType := FontType or WRITABLE_FONTTYPE;
      Items.AddObject(StrPas(LogFont.lfFaceName), TObject(FontType));
    end;
  Result := 1;
end;

{$ENDIF WIN32}

constructor TJvxFontComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTrueTypeBMP := CreateBitmap('JVTRUETYPE_FNT');
  FDeviceBMP := CreateBitmap('JVDEVICE_FNT');
  FDevice := fdScreen;
  Sorted := True;
  inherited ItemHeight := MinItemHeight;
end;

destructor TJvxFontComboBox.Destroy;
begin
  FTrueTypeBMP.Free;
  FDeviceBMP.Free;
  inherited Destroy;
end;

procedure TJvxFontComboBox.CreateWnd;
var
  OldFont: TFontName;
begin
  OldFont := FontName;
  inherited CreateWnd;
  FUpdate := True;
  try
    PopulateList;
    inherited Text := '';
    SetFontName(OldFont);
  finally
    FUpdate := False;
  end;
  if AnsiCompareText(FontName, OldFont) <> 0 then DoChange;
end;

procedure TJvxFontComboBox.PopulateList;
var
  DC: HDC;
{$IFNDEF WIN32}
  Proc: TFarProc;
{$ENDIF}
begin
  if not HandleAllocated then Exit;
  Items.BeginUpdate;
  try
    Clear;
    DC := GetDC(0);
    try
{$IFDEF WIN32}
      if (FDevice = fdScreen) or (FDevice = fdBoth) then
        EnumFontFamilies(DC, nil, @EnumFontsProc, Longint(Self));
      if (FDevice = fdPrinter) or (FDevice = fdBoth) then
      try
        EnumFontFamilies(Printer.Handle, nil, @EnumFontsProc, Longint(Self));
      except
        { skip any errors }
      end;
{$ELSE}
      Proc := MakeProcInstance(@EnumFontsProc, HInstance);
      try
        if (FDevice = fdScreen) or (FDevice = fdBoth) then
          EnumFonts(DC, nil, Proc, PChar(Self));
        if (FDevice = fdPrinter) or (FDevice = fdBoth) then
          try
            EnumFonts(Printer.Handle, nil, Proc, PChar(Self));
          except
            { skip any errors }
          end;
      finally
        FreeProcInstance(Proc);
      end;
{$ENDIF}
    finally
      ReleaseDC(0, DC);
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TJvxFontComboBox.SetFontName(const NewFontName: TFontName);
var
  Item: Integer;
begin
  if FontName <> NewFontName then begin
    if not (csLoading in ComponentState) then begin
      HandleNeeded;
      { change selected item }
      for Item := 0 to Items.Count - 1 do
        if AnsiCompareText(Items[Item], NewFontName) = 0 then begin
          ItemIndex := Item;
          DoChange;
          Exit;
        end;
      if Style = csDropDownList then ItemIndex := -1
      else inherited Text := NewFontName;
    end
    else inherited Text := NewFontName;
    DoChange;
  end;
end;

function TJvxFontComboBox.GetFontName: TFontName;
begin
  Result := inherited Text;
end;

function TJvxFontComboBox.GetTrueTypeOnly: Boolean;
begin
  Result := foTrueTypeOnly in FOptions;
end;

procedure TJvxFontComboBox.SetOptions(Value: TFontListOptions);
begin
  if Value <> Options then begin
    FOptions := Value;
    Reset;
  end;
end;

procedure TJvxFontComboBox.SetTrueTypeOnly(Value: Boolean);
begin
  if Value <> TrueTypeOnly then begin
    if Value then FOptions := FOptions + [foTrueTypeOnly]
    else FOptions := FOptions - [foTrueTypeOnly];
    Reset;
  end;
end;

procedure TJvxFontComboBox.SetDevice(Value: TFontDevice);
begin
  if Value <> FDevice then begin
    FDevice := Value;
    Reset;
  end;
end;

procedure TJvxFontComboBox.SetUseFonts(Value: Boolean);
begin
  if Value <> FUseFonts then begin
    FUseFonts := Value;
    Invalidate;
  end;
end;

procedure TJvxFontComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Bitmap: TBitmap;
  BmpWidth: Integer;
  Text: array[0..255] of Char;
begin
  with Canvas do begin
    FillRect(Rect);
    BmpWidth  := 20;
    if (Integer(Items.Objects[Index]) and TRUETYPE_FONTTYPE) <> 0 then
      Bitmap := FTrueTypeBMP
    else if (Integer(Items.Objects[Index]) and DEVICE_FONTTYPE) <> 0 then
      Bitmap := FDeviceBMP
    else Bitmap := nil;
    if Bitmap <> nil then begin
      BmpWidth := Bitmap.Width;
      BrushCopy(Bounds(Rect.Left + 2, (Rect.Top + Rect.Bottom - Bitmap.Height)
        div 2, Bitmap.Width, Bitmap.Height), Bitmap, Bounds(0, 0, Bitmap.Width,
        Bitmap.Height), Bitmap.TransparentColor);
    end;
    { uses DrawText instead of TextOut in order to get clipping against
      the combo box button }
    {TextOut(Rect.Left + bmpWidth + 6, Rect.Top, Items[Index])}
    StrPCopy(Text, Items[Index]);
    Rect.Left := Rect.Left + BmpWidth + 6;
    if FUseFonts and (Integer(Items.Objects[Index]) and WRITABLE_FONTTYPE <> 0) then
      Font.Name := Items[Index];
    DrawText(Handle, Text, StrLen(Text), Rect,
{$IFDEF Delphi4_Up}
      DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX));
{$ELSE}
      DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
{$ENDIF}
  end;
end;

procedure TJvxFontComboBox.WMFontChange(var Message: TMessage);
begin
  inherited;
  Reset;
end;

function TJvxFontComboBox.MinItemHeight: Integer;
begin
  Result := inherited MinItemHeight;
  if Result < FTrueTypeBMP.Height - 1 then
    Result := FTrueTypeBMP.Height - 1;
end;

procedure TJvxFontComboBox.Change;
var
  I: Integer;
begin
  inherited Change;
  if Style <> csDropDownList then begin
    I := Items.IndexOf(inherited Text);
    if (I >= 0) and (I <> ItemIndex) then begin
      ItemIndex := I;
      DoChange;
    end;
  end;
end;

procedure TJvxFontComboBox.Click;
begin
  inherited Click;
  DoChange;
end;

procedure TJvxFontComboBox.DoChange;
begin
  if not (csReading in ComponentState) then
    if not FUpdate and Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJvxFontComboBox.Reset;
var
  SaveName: TFontName;
begin
  if HandleAllocated then begin
    FUpdate := True;
    try
      SaveName := FontName;
      PopulateList;
      FontName := SaveName;
    finally
      FUpdate := False;
      if FontName <> SaveName then DoChange;
    end;
  end;
end;

end.
