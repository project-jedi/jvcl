{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorCombo.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ Comboboxes for displaying colors and fonts }

unit JvColorCombo;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Dialogs, Graphics, StdCtrls, Printers, JvComponent;

type
{ TJvColorComboBox }

  TJvNewColorEvent=procedure (Sender:TObject;Color:TColor;var DisplayName:string;var AllowAdd:boolean) of object;
  TJvColorComboOption=(coText,coHex,coRGB,coSysColors,coCustomColors);
  TJvColorComboOptions = set of TJvColorComboOption;

  TJvColorComboBox = class(TJvCustomComboBox)
  private
    FColorValue: TColor;
    FCustCnt:integer;
    FHiLiteColor  :TColor;
    FHiLiteText   :TColor;
    FOptions      :TJvColorComboOptions;
    FPrefix       :string;
    FOther        :string;
    FColWidth     :integer;
    FNewColor     :TJvNewColorEvent;
    procedure SetOptions(Value:TJvColorComboOptions);
    procedure SetOther(Value:string);
    procedure SetColWidth(Value:integer);
    procedure SetColorValue(Value: TColor);
    procedure SetDroppedWidth(Value:integer);
    function GetDroppedWidth:integer;
    procedure ResetItemHeight;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure SetPrefix(const Value: string);
  protected
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; R: TRect; State: TOwnerDrawState); override;
    procedure Click; override;
    procedure GetColors; virtual;
    function DoNewColor(Color:TColor;var DisplayName:string):boolean;virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property Text;
  published
    property Anchors;
    property Constraints;

    property ColorValue: TColor read FColorValue write SetColorValue default clBlack;
    property ColorDialogText:string read FOther write SetOther;
    property ColorWidth:integer read FColWidth write SetColWidth default 21;
    property NewColorText:string read FPrefix write SetPrefix;
    property Options:TJvColorComboOptions read FOptions write SetOptions default [coText];
//    property DisplayNames: Boolean read FDisplayNames write SetDisplayNames default True;
//    property AsHex:boolean read FAsHex write SetAsHex default false;
    property DroppedDownWidth:integer read GetDroppedWidth write SetDroppedWidth;
    property HiliteColor:TColor read FHiliteColor write FHiLiteColor default clHighLight;
    property HiliteText:TColor read FHiliteText write FHiLiteText default clHighLightText;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnNewColor:TJvNewColorEvent read FNewColor write FNewColor;
    property OnChange;
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
    property OnStartDrag;
  end;

{ TJvFontComboBox2 }

//  TFontDialogDevice = (fdScreen, fdPrinter, fdBoth); { already in Dialogs }
  TJvFontComboOption = (foAnsiOnly, foTrueTypeOnly, foFixedPitchOnly,
    foNoOEMFonts, foOEMFontsOnly, foScalableOnly,foWysiWyg);
  TJvFontComboOptions = set of TJvFontComboOption;

  TJvFontComboBox2 = class(TJvCustomComboBox)
  private
    TrueTypeBMP, FixBMP,DeviceBMP: TBitmap;
    FDevice: TFontDialogDevice;
    FHiLiteColor:TColor;
    FHiLiteText:TColor;
    FUseImages:boolean;
    FOptions: TJvFontComboOptions;
    procedure SetUseImages(Value:boolean);
    procedure SetDevice(Value: TFontDialogDevice);
    procedure SetOptions(Value: TJvFontComboOptions);
    procedure ResetItemHeight;
    procedure Reset;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMFontChange(var Message: TMessage); message WM_FONTCHANGE;
    function GetFontName: string;
    procedure SetFontName(const Value: string);
  protected
    procedure GetFonts; virtual;
    procedure Click; override;
    procedure CreateWnd; override;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure DrawItem(Index: Integer; R: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Text;
  published
    property Anchors;
    property Constraints;
    property Color;
    property Ctl3D;
    property FontName:string read GetFontName write SetFontName;
    property Device: TFontDialogDevice read FDevice write SetDevice default fdBoth;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property ItemIndex;
    property HiliteColor:TColor read FHiliteColor write FHiLiteColor default clHighLight;
    property HiliteText:TColor read FHiliteText write FHiLiteText default clHighLightText;
    property Options: TJvFontComboOptions read FOptions write SetOptions default [];
    property UseImages:boolean read FUseImages write SetUseImages default true;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
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
    property OnStartDrag;
  end;


resourcestring
  SOtherCaption = '(Other...)';
  SNewColorPrefix = 'New Color ';

implementation
{$R *.RES}

const
  ColCount = 16;
  SysColCount = 25;
  ColorValues: array [1..ColCount] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);

  SysColorValues:array [1..SysColCount] of TColor = (
    clScrollBar, clBackground, clActiveCaption, clInactiveCaption, clMenu,
    clWindow, clWindowFrame, clMenuText, clWindowText, clCaptionText, clActiveBorder,
    clInactiveBorder, clAppWorkSpace, clHighlight, clHighlightText, clBtnFace,
    clBtnShadow, clGrayText, clBtnText, clInactiveCaptionText, clBtnHighlight,
    cl3DDkShadow, cl3DLight, clInfoText, clInfoBk);


function LoadInternalBitmap(ResName:string):TBitmap;
begin
  Result := TBitmap.Create;
  Result.Handle := LoadBitmap(hInstance,PChar(ResName));
end;

function GetItemHeight(Font: TFont): Integer;
var
  DC: HDC;
  aFont: HFont;
  TM: TTextMetric;
begin
  DC := GetDC(0);
  try
    aFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, TM);
    SelectObject(DC, aFont);
  finally
    ReleaseDC(0, DC);
  end;
  Result := TM.tmHeight + 1;
end;

function Max(Val1,Val2:integer):integer;
begin
  Result := Val1;
  if Val2 > Val1 then Result := Val2;
end;

function IncludeFont(Options:TJvFontComboOptions;LogFont:TLogFont;FontType:integer): Boolean;
begin
  Result := True;
  if foAnsiOnly in Options then
    Result := Result and (LogFont.lfCharSet = ANSI_CHARSET);
  if foTrueTypeOnly in Options then
    Result := Result and (FontType and TRUETYPE_FONTTYPE > 0);
  if foFixedPitchOnly in Options then
    Result := Result and (LogFont.lfPitchAndFamily and FIXED_PITCH > 0);
  if foOEMFontsOnly in Options then
    Result := Result and (LogFont.lfCharSet = OEM_CHARSET);
  if foNoOEMFonts in Options then
    Result := Result and (LogFont.lfCharSet <> OEM_CHARSET);
  if foScalableOnly in Options then
    Result := Result and (FontType and RASTER_FONTTYPE = 0);
end;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
var
  aC: TJvFontComboBox2;
begin
  aC := TJvFontComboBox2(Data);
  Result := 0;
  if aC = nil then Exit;
  if IncludeFont(aC.Options,LogFont,FontType) then
  begin
    if aC.Items.IndexOf(string(LogFont.lfFaceName)) = -1 then
      aC.Items.AddObject(string(LogFont.lfFaceName), TObject(FontType));
  end;
  Result := 1;
end;

{ TJvColorComboBox }

constructor TJvColorComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csOwnerDrawFixed;
  FColorValue := clBlack;
  FColWidth    := 21;
  FPrefix      := SNewColorPrefix;
  FOther       := SOtherCaption;
  FOptions      := [coCustomColors,coText];
  FHiLiteColor  := clHighLight;
  FHiLiteText   := clHighLightText;
end;

procedure TJvColorComboBox.GetColors;
var
  i: integer;
  ColorName: string;
begin
  Clear;
  for i := 1 to ColCount do
  begin
    ColorName := Copy(ColorToString(ColorValues[i]), 3, 40);
    Items.AddObject(ColorName, TObject(ColorValues[i]));
  end;
  if (coSysColors in FOptions) then
    for i := 1 to SysColCount do
    begin
      ColorName := Copy(ColorToString(SysColorValues[i]), 3, 40);
      Items.AddObject(ColorName, TObject(SysColorValues[i]));
    end;
  if (coCustomColors in FOptions) then
    Items.AddObject(FOther,TObject($808080));
  SetColorValue(FColorValue);
end;

procedure TJvColorComboBox.SetDroppedWidth(Value:integer);
begin
  SendMessage(Handle,CB_SETDROPPEDWIDTH,Value,0);
end;

function TJvColorComboBox.GetDroppedWidth:integer;
begin
  Result := SendMessage(Handle,CB_GETDROPPEDWIDTH,0,0);
end;

procedure TJvColorComboBox.SetOptions(Value:TJvColorComboOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    Invalidate;
  end;
end;

procedure TJvColorComboBox.SetOther(Value:string);
begin
  FOther := Value;
  GetColors;
end;

procedure TJvColorComboBox.SetColWidth(Value:integer);
begin
  if FColWidth <> Value then
  begin
    FColWidth := Value;
    Invalidate;
  end;
end;

procedure TJvColorComboBox.SetColorValue(Value: TColor);
var
  i: Integer;
  aColor: TColor;
begin
  if (ItemIndex < 0) or (Value <> FColorValue) then
  begin
    for i := 0 to Items.Count - 1 do
    begin
      aColor := TColor(Items.Objects[i]);
      if aColor = Value then
      begin
        FColorValue := Value;
        if ItemIndex <> i then ItemIndex := i;
        Change;
        Exit;
      end;
    end;
    if (coCustomColors in Options) then
      Items.InsertObject(Items.Count - 1,FPrefix + IntToStr(FCustCnt),TObject(Value))
    else
      Items.AddObject(FPrefix + IntToStr(FCustCnt),TObject(Value));
    Inc(FCustCnt);
    ItemIndex := Items.Count - 2;
    ColorValue := Value;
  end;
end;

procedure TJvColorComboBox.CreateWnd;
begin
  inherited CreateWnd;
  GetColors;
  SetColorValue(FColorValue);
end;

function TJvColorComboBox.DoNewColor(Color:TColor;var DisplayName:string):boolean;
begin
  Result := true;
  if Assigned(FNewColor) then FNewColor(self,Color,DisplayName,Result);
end;

procedure TJvColorComboBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := [];
    if bool(itemState and ODS_CHECKED) then
      Include(State,odChecked);
    if bool(itemState and ODS_COMBOBOXEDIT) then
      Include(State,odComboBoxEdit);
    if bool(itemState and ODS_DEFAULT) then
      Include(State,odDefault);
    if bool(itemState and ODS_DISABLED) then
      Include(State,odDisabled);
    if bool(itemState and ODS_FOCUS) then
      Include(State,odFocused);
    if bool(itemState and ODS_GRAYED) then
      Include(State,odGrayed);
    if bool(itemState and ODS_SELECTED) then
      Include(State,odSelected);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      Canvas.Brush.Color := FHiLiteColor;
      Canvas.Font.Color := FHiLiteText;
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
      Canvas.FillRect(rcItem);
    Canvas.Handle := 0;
  end;
end;

procedure TJvColorComboBox.DrawItem(Index: Integer; R: TRect;
  State: TOwnerDrawState);
var
  aRect: TRect;
  aColor: TColor;
  S:string;
begin
  aRect := R;
  Inc(aRect.Top, 2);
  Inc(aRect.Left, 2);
  Dec(aRect.Bottom, 2);
  if (coText in FOptions) or (coHex in FOptions) or (coRGB in FOptions) or
    ((coCustomColors in FOptions) and (Index = Items.Count - 1)) then
    aRect.Right := aRect.Left + FColWidth
  else
    Dec(aRect.Right, 3);

  with Canvas do
  begin
    aColor := Brush.Color;
    Brush.Color := Color;
    FillRect(R);
    Brush.Color := clGray;
    OffsetRect(aRect,2,2);
    FillRect(aRect);
    OffsetRect(aRect,-2,-2);
    Brush.Color := TColor(Items.Objects[Index]);
    try
      Rectangle(aRect);
    finally
      Brush.Color := aColor;
    end;
    if (coText in FOptions) or (coHex in FOptions) or (coRGB in FOptions) or
      ((coCustomColors in FOptions) and (Index = Items.Count - 1)) then
//    if (FOption in [coText,coHex,coRGB]) or (FCustomColor and (Index = Items.Count - 1)) then
    begin
      S := Items[Index];
      if (S <> FOther) then
      begin
        if (coHex in FOptions) then
          S := Format('0x%.6x',[ColorToRGB(TColor(Items.Objects[Index]))])
        else if (coRGB in Foptions) then
          S := Format('(%d,%d,%d)',[GetRValue(TColor(Items.Objects[Index])),GetGValue(TColor(Items.Objects[Index])),GetBValue(TColor(Items.Objects[Index]))]);
      end;

      R.Left := R.Left + FColWidth + 6;
      R.Right := R.Left + TextWidth(S) + 6;
      FillRect(R);
      OffsetRect(R,2,0);
      SetBkMode(Canvas.Handle,TRANSPARENT);
      DrawText(Canvas.Handle, PChar(S), -1, R, DT_SINGLELINE or
        DT_VCENTER or DT_NOPREFIX);
      OffsetRect(R,-2,0);
    end
    else
      FrameRect(R);
    if odSelected in State then
       DrawFocusRect(R);
  end;
end;

procedure TJvColorComboBox.Click;
var S:string;
begin
  if (ItemIndex = Items.Count - 1) and (coCustomColors in FOptions) then
    with TColorDialog.Create(nil) do
    begin
      Color := ColorValue;
      Options := Options + [cdFullOpen,cdPreventFullOpen];
      S := FPrefix + IntToStr(FCustCnt);
      if Execute and DoNewColor(Color,S) then
      begin
        Inc(FCustCnt);
        Items.InsertObject(Items.Count - 1,S,TObject(Color));
        ItemIndex := Items.Count - 2;
        ColorValue := Color;
      end;
      Free;
    end    // with
  else if ItemIndex >= 0 then
    ColorValue := TColor(Items.Objects[ItemIndex]);
  inherited Click;
end;

procedure TJvColorComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvColorComboBox.ResetItemHeight;
begin
  ItemHeight := Max(GetItemHeight(Font), 9);
end;

procedure TJvColorComboBox.SetPrefix(const Value: string);
begin
  FPrefix := Value;
end;

{ TJvFontComboBox2 }


constructor TJvFontComboBox2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TrueTypeBMP := LoadInternalBitmap('TTF_FONT');
  FixBMP    := LoadInternalBitmap('FIX_FONT');
  DeviceBMP := LoadInternalBitmap('PRN_FONT');
  FHiliteColor := clHighLight;
  FHiLiteText := clHighLightText;
  FDevice := fdBoth;
  FUseImages := true;
  Style := csOwnerDrawFixed;
//  Sorted := True;
  ResetItemHeight;
end;

destructor TJvFontComboBox2.Destroy;
begin
  TrueTypeBMP.Free;
  DeviceBMP.Free;
  FixBMP.Free;
  inherited Destroy;
end;

procedure TJvFontComboBox2.GetFonts;
var
  DC: HDC;
  Proc: TFarProc;
begin
  if not HandleAllocated {or (csDesigning in ComponentState) }then Exit;
  Clear;
  DC := GetDC(0);
  try
    Proc := @EnumFontsProc; 
    try
      if FDevice in [fdScreen,fdBoth] then
        EnumFonts(DC, nil, Proc, Pointer(Self));
      if FDevice in [fdPrinter,fdBoth] then
        try
          EnumFonts(Printer.Handle, nil, Proc, Pointer(Self));
        except
        end;
    finally
//      FreeProcInstance(Proc);
    end;
  finally
    ReleaseDC(0, DC);
  end;
  ItemIndex := 0;
end;


procedure TJvFontComboBox2.SetOptions(Value: TJvFontComboOptions);
begin
  if Value <> Options then
  begin
    FOptions := Value;
    Reset;
  end;
end;

procedure TJvFontComboBox2.SetUseImages(Value:boolean);
begin
  if FUseImages <> Value then
  begin
    FUseImages := Value;
    Invalidate;
  end;
end;

procedure TJvFontComboBox2.SetDevice(Value: TFontDialogDevice);
begin
  if Value <> FDevice then
  begin
    FDevice := Value;
    Reset;
  end;
end;

procedure TJvFontComboBox2.CreateWnd;
begin
  inherited CreateWnd;
  GetFonts;
end;

procedure TJvFontComboBox2.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := [];
    if bool(itemState and ODS_CHECKED) then
      Include(State,odChecked);
    if bool(itemState and ODS_COMBOBOXEDIT) then
      Include(State,odComboBoxEdit);
    if bool(itemState and ODS_DEFAULT) then
      Include(State,odDefault);
    if bool(itemState and ODS_DISABLED) then
      Include(State,odDisabled);
    if bool(itemState and ODS_FOCUS) then
      Include(State,odFocused);
    if bool(itemState and ODS_GRAYED) then
      Include(State,odGrayed);
    if bool(itemState and ODS_SELECTED) then
      Include(State,odSelected);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      Canvas.Brush.Color := FHiliteColor;
      Canvas.Font.Color := FHiliteText;
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
      Canvas.FillRect(rcItem);
    Canvas.Handle := 0;
  end;
end;

procedure TJvFontComboBox2.DrawItem(Index: Integer; R: TRect;
  State: TOwnerDrawState);
var
  aBmp: TBitmap;
  aColor:TColor;
  aWidth: Integer;
  aName:string;
begin
  with Canvas do
  begin
    aColor := Brush.Color;
    Brush.Color := Color;
    FillRect(R);
//    aWidth  := 20;
    if (Integer(Items.Objects[Index]) and TRUETYPE_FONTTYPE) <> 0 then
      aBmp := TrueTypeBMP
    else if (Integer(Items.Objects[Index]) and DEVICE_FONTTYPE) <> 0 then
      aBmp := DeviceBMP
    else
      aBmp := FixBMP;
    if not FUseImages then aBmp := nil;

    if aBmp <> nil then
    begin
      aWidth := aBmp.Width;
      BrushCopy(Bounds(R.Left + 2, (R.Top + R.Bottom - aBmp.Height) div 2,
           aBmp.Width, aBmp.Height), aBmp, Bounds(0, 0, aBmp.Width, aBmp.Height), clFuchsia);
      R.Left := R.Left + aWidth + 6;
    end;
    Brush.Color := aColor;
    aName := Canvas.Font.Name;
    if foWysiwyg in FOptions then
      Canvas.Font.Name := Items[Index];
    R.Right := R.Left + TextWidth(Items[Index]) + 6;
    FillRect(R);
    OffsetRect(R,2,0);
    DrawText(Canvas.Handle, PChar(Items[Index]), -1, R, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    Canvas.Font.Name := aName;
    OffsetRect(R,-2,0);
    if odSelected in State then
      DrawFocusRect(R);
  end;
end;

procedure TJvFontComboBox2.WMFontChange(var Message: TMessage);
begin
  inherited;
  Reset;
end;

procedure TJvFontComboBox2.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TJvFontComboBox2.ResetItemHeight;
begin
  ItemHeight := Max(GetItemHeight(Font), TrueTypeBMP.Height);
end;

procedure TJvFontComboBox2.Click;
begin
  inherited Click;
  Change;
end;

procedure TJvFontComboBox2.Reset;
 var i:integer;
begin
  if HandleAllocated then
  begin
    i := ItemIndex;
    GetFonts;
    ItemIndex := i;
  end;
end;


function TJvFontComboBox2.GetFontName: string;
begin
  Result := Text;
end;

procedure TJvFontComboBox2.SetFontName(const Value: string);
begin
  ItemIndex := Items.IndexOf(Value);
end;

end.
