{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSpin.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Polaris Software
  boerema1
  roko
  remkobonte

Last Modified: 2003-03-17

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

-----------------------------------------------------------------------------}

{$I jvcl.inc}

//>Polaris
{$DEFINE POLESPIN} {Classic style in JvSpinButton and JvSpinEdit}
//<Polaris

unit JvSpin;

interface

uses
  Windows, ComCtrls, Controls, ExtCtrls, Classes, Graphics, Messages, Forms,
  StdCtrls, Menus, SysUtils,
  JvEdit, JvMaskEdit, JvComponent;

type
  TSpinButtonState = (sbNotDown, sbTopDown, sbBottomDown);

  TJvSpinButtonStyle = (sbsDefault, sbsClassic); // Polaris

  TJvSpinButton = class(TJvGraphicControl)
  private
    FDown: TSpinButtonState;
    FDragging: Boolean;
    FInvalidate: Boolean;

    { Custom up arrow: }
    FUpBitmap: TBitmap;
    { Custom down arrow: }
    FDownBitmap: TBitmap;
    { Buffered buttons with various states: }
    FTopDownBtn: TBitmap;
    FBottomDownBtn: TBitmap;
    FNotDownBtn: TBitmap;
    {$IFDEF JVCLThemesEnabled}
    FTopHotBtn: TBitmap;
    FBottomHotBtn: TBitmap;
    FMouseInTopBtn: Boolean;
    FMouseInBottomBtn: Boolean;
    {$ENDIF}

    FRepeatTimer: TTimer;
    FLastDown: TSpinButtonState;
    FFocusControl: TWinControl;
    FOnTopClick: TNotifyEvent;
    FOnBottomClick: TNotifyEvent;
    //>Polaris
    FButtonStyle: TJvSpinButtonStyle;
    procedure SetButtonStyle(Value: TJvSpinButtonStyle);
    //<Polaris
    procedure TopClick;
    procedure BottomClick;
    procedure GlyphChanged(Sender: TObject);
    function GetUpGlyph: TBitmap;
    function GetDownGlyph: TBitmap;
    procedure SetUpGlyph(Value: TBitmap);
    procedure SetDownGlyph(Value: TBitmap);
    procedure SetDown(Value: TSpinButtonState);
    procedure SetFocusControl(Value: TWinControl);

    procedure DrawAllBitmap;
    procedure DrawBitmap(ABitmap: TBitmap; ADownState: TSpinButtonState);
    {$IFDEF JVCLThemesEnabled}
    procedure DrawAllBitmapClassicThemed;
    procedure DrawAllBitmapDiagonalThemed;
    {$ENDIF}

    procedure TimerExpired(Sender: TObject);
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
  protected
    procedure EnabledChanged; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    function MouseInBottomBtn(const P: TPoint): Boolean;
    {$IFDEF JVCLThemesEnabled}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    {$ENDIF JVCLThemesEnabled}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Down: TSpinButtonState read FDown write SetDown default sbNotDown;
  published
    //>Polaris
    property ButtonStyle: TJvSpinButtonStyle read FButtonStyle write SetButtonStyle default sbsDefault;
    //<Polaris
    property DragCursor;
    property DragMode;
    property Enabled;
    property Visible;
    property DownGlyph: TBitmap read GetDownGlyph write SetDownGlyph;
    property UpGlyph: TBitmap read GetUpGlyph write SetUpGlyph;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property ShowHint;
    property ParentShowHint;
    property Anchors;
    property Constraints;
    property DragKind;
    property OnBottomClick: TNotifyEvent read FOnBottomClick write FOnBottomClick;
    property OnTopClick: TNotifyEvent read FOnTopClick write FOnTopClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

  {$IFDEF BCB}
  TValueType = (vtInt, vtFloat, vtHex);
  {$ELSE}
  TValueType = (vtInteger, vtFloat, vtHex);
  {$ENDIF BCB}

  TSpinButtonKind = (bkStandard, bkDiagonal, bkClassic);

  TJvCheckOption = (coCheckOnChange, coCheckOnExit, coCropBeyondLimit);
  TJvCheckOptions = set of TJvCheckOption;

  //>Polaris
  //  TJvSpinEdit = class(TCustomEdit)
  TJvCustomSpinEdit = class(TJvCustomMaskEdit)
  private
    //Polaris
    FCheckMaxValue: Boolean;
    FCheckMinValue: Boolean;
    FCheckOptions: TJvCheckOptions;
    FDisplayFormat: string;
    FFocused: Boolean;
    FLCheckMaxValue: Boolean;
    FLCheckMinValue: Boolean;
    //<Polaris
    FAlignment: TAlignment;
    FMinValue: Extended;
    FMaxValue: Extended;
    FOldValue: Extended;
    FIncrement: Extended;
    FDecimal: Byte;
    FChanging: Boolean;
    //FOldValue: Extended; // New
    FEditorEnabled: Boolean;
    FValueType: TValueType;
    FButton: TJvSpinButton;
    FBtnWindow: TWinControl;
    FArrowKeys: Boolean;
    FOnTopClick: TNotifyEvent;
    FOnBottomClick: TNotifyEvent;
    // FButtonKind: TSpinButtonKind;
    FUpDown: TCustomUpDown;
    //Polaris
    FThousands: Boolean; // New

    //Polaris
    function StoreCheckMaxValue: Boolean;
    function StoreCheckMinValue: Boolean;
    procedure SetCheckMaxValue(NewValue: Boolean);
    procedure SetCheckMinValue(NewValue: Boolean);
    procedure SetMaxValue(NewValue: Extended);
    procedure SetMinValue(NewValue: Extended);

    function CheckDefaultRange(CheckMax: Boolean): Boolean;
    procedure SetDisplayFormat(const Value: string);
    function IsFormatStored: Boolean;
    //function TextToValText(const AValue: string): string;
    //Polaris
    procedure SetFocused(Value: Boolean);
    //procedure CheckRange(const AOption: TJvCheckOption);

    //function TryGetValue(var Value: Extended): Boolean; // New
    function GetAsInteger: Longint;
    function GetButtonKind: TSpinButtonKind;
    function GetButtonWidth: Integer;
    function GetMinHeight: Integer;
    function IsIncrementStored: Boolean;
    function IsMaxStored: Boolean;
    function IsMinStored: Boolean;
    function IsValueStored: Boolean;
    procedure GetTextHeight(var SysHeight, Height: Integer);
    procedure ResizeButton;
    procedure SetAlignment(Value: TAlignment);
    procedure SetArrowKeys(Value: Boolean);
    procedure SetAsInteger(NewValue: Longint);
    procedure SetButtonKind(Value: TSpinButtonKind);
    procedure SetDecimal(NewValue: Byte);
    procedure SetEditRect;
    procedure SetThousands(const Value: Boolean);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure CMBiDiModeChanged(var Msg: TMessage); message CM_BIDIMODECHANGED;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure WMCut(var Msg: TWMCut); message WM_CUT;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMPaste(var Msg: TWMPaste); message WM_PASTE;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  protected
    //Polaris up to protected
    FButtonKind: TSpinButtonKind;
    procedure EnabledChanged; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure FontChanged; override;
    function CheckValue(NewValue: Extended): Extended;
    function CheckValueRange(NewValue: Extended; RaiseOnError: Boolean): Extended;
    function GetValue: Extended; virtual; abstract;
    procedure DataChanged; virtual;
    procedure RecreateButton;
    procedure SetValue(NewValue: Extended); virtual; abstract;
    procedure SetValueType(NewType: TValueType); virtual;
    //Polaris up to protected

    //Polaris
    procedure Loaded; override;
    function DefaultDisplayFormat: string; virtual;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat stored IsFormatStored;
    //    procedure DefinePropertyes(Filer: TFiler); override;
    //Polaris

    function IsValidChar(Key: Char): Boolean; virtual;
    procedure Change; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DownClick(Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure UpClick(Sender: TObject); virtual;
    property ButtonWidth: Integer read GetButtonWidth;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AsInteger: Longint read GetAsInteger write SetAsInteger default 0;
    property Text;
    //Polaris  published
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property ArrowKeys: Boolean read FArrowKeys write SetArrowKeys default True;
    property ButtonKind: TSpinButtonKind read FButtonKind write SetButtonKind default bkDiagonal;
    property Decimal: Byte read FDecimal write SetDecimal default 2;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Increment: Extended read FIncrement write FIncrement stored IsIncrementStored;
    property MaxValue: Extended read FMaxValue write SetMaxValue stored IsMaxStored;
    property MinValue: Extended read FMinValue write SetMinValue stored IsMinStored;
    //Polaris
    //property CheckOnExit: Boolean read FCheckOnExit write FCheckOnExit default False;
    //property CheckOnChange: Boolean read FCheckOnChange write FCheckOnChange default False;
    property CheckOptions: TJvCheckOptions read FCheckOptions write FCheckOptions default [coCheckOnChange, coCheckOnExit, coCropBeyondLimit];
    property CheckMinValue: Boolean read FCheckMinValue write SetCheckMinValue stored StoreCheckMinValue;
    //default False;
    property CheckMaxValue: Boolean read FCheckMaxValue write SetCheckMaxValue stored StoreCheckMaxValue;
    //default False;
    //Polaris
    property ValueType: TValueType read FValueType write SetValueType
      default {$IFDEF BCB} vtInt {$ELSE} vtInteger {$ENDIF};
    property Value: Extended read GetValue write SetValue stored IsValueStored;
    property Thousands: Boolean read FThousands write SetThousands default False;
    property OnBottomClick: TNotifyEvent read FOnBottomClick write FOnBottomClick;
    property OnTopClick: TNotifyEvent read FOnTopClick write FOnTopClick;
  end;

  TJvSpinEdit = class(TJvCustomSpinEdit)
  protected
    procedure SetValue(NewValue: Extended); override;
    function GetValue: Extended; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    //Polaris
    //property CheckOnExit;
    property CheckOptions;
    property CheckMinValue;
    property CheckMaxValue;

    property Align;
    property Alignment;
    property ArrowKeys;
    property DisplayFormat;
    property ButtonKind default bkDiagonal;
    property Thousands;
    property Decimal;
    property EditorEnabled;
    property Increment;
    property MaxValue;
    property MinValue;
    property ValueType;
    property Value;
    property OnBottomClick;
    property OnTopClick;

    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnEndDock;
    property OnStartDock;
    property AboutJVCL;
    property HideSelection;
    property HotTrack;
    {$IFDEF COMPILER6_UP}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    {$ENDIF}
    (* ++ RDB ++ *)
    property ClipboardCommands;
    property DisabledTextColor;
    property DisabledColor;
    (* -- RDB -- *)
  end;

implementation

uses
  Consts, CommCtrl,
  JvThemes,
  {$IFDEF JVCLThemesEnabled}
  UxTheme, {$IFNDEF COMPILER7_UP}TmSchema,{$ENDIF}
  {$ENDIF}
  JvJCLUtils, JvConsts, JvResources;

{$R ..\resources\JvSpin.Res}

const
  sSpinUpBtn = 'JVSPINUP';
  sSpinDownBtn = 'JVSPINDOWN';
  sSpinUpBtnPole = 'JVSPINUPPOLE';
  sSpinDownBtnPole = 'JVSPINDOWNPOLE';

const
  InitRepeatPause = 400; { pause before repeat timer (ms) }
  RepeatPause = 100;

function DefBtnWidth: Integer;
begin
  Result := GetSystemMetrics(SM_CXVSCROLL);
  if Result > 15 then
    Result := 15;
end;

function RemoveThousands(const AValue: string): string;
begin
  if DecimalSeparator <> ThousandSeparator then
    Result := DelChars(AValue, ThousandSeparator)
  else
    Result := AValue;
end;

function TextToValText(const AValue: string): string;
begin
  Result := DelRSpace(AValue);
  if DecimalSeparator <> ThousandSeparator then
    Result := DelChars(Result, ThousandSeparator);

  if (DecimalSeparator <> '.') and (ThousandSeparator <> '.') then
    Result := ReplaceStr(Result, '.', DecimalSeparator);
  if (DecimalSeparator <> ',') and (ThousandSeparator <> ',') then
    Result := ReplaceStr(Result, ',', DecimalSeparator);

  if Result = '' then
    Result := '0'
  else
  if Result = '-' then
    Result := '-0';
end;

//=== TJvSpinButton ==========================================================

procedure TJvSpinButton.BottomClick;
begin
  if Assigned(FOnBottomClick) then
  begin
    FOnBottomClick(Self);
    if not (csLButtonDown in ControlState) then
      FDown := sbNotDown;
  end;
end;

procedure TJvSpinButton.EnabledChanged;
begin
  inherited EnabledChanged;
  //>Polaris
  //  FInvalidate := True;
  //  Invalidate;
  GlyphChanged(Self);
  //<Polaris
end;

procedure TJvSpinButton.CMSysColorChange(var Msg: TMessage);
begin
  inherited;
  { The buttons we draw are buffered, thus we need to repaint them to theme changes etc. }
  GlyphChanged(Self);
end;

constructor TJvSpinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF POLESPIN}
  FButtonStyle := sbsDefault;
  {$ENDIF}
  FUpBitmap := TBitmap.Create;
  FDownBitmap := TBitmap.Create;
  //FUpBitmap.Handle := LoadBitmap(HInstance, sSpinUpBtn); // Polaris
  //FDownBitmap.Handle := LoadBitmap(HInstance, sSpinDownBtn); // Polaris
  FUpBitmap.OnChange := GlyphChanged;
  FDownBitmap.OnChange := GlyphChanged;
  Height := 20;
  Width := 20;
  FTopDownBtn := TBitmap.Create;
  FBottomDownBtn := TBitmap.Create;
  FNotDownBtn := TBitmap.Create;
  {$IFDEF JVCLThemesEnabled}
  FTopHotBtn := TBitmap.Create;
  FBottomHotBtn := TBitmap.Create;
  {$ENDIF}
  DrawAllBitmap;
  FLastDown := sbNotDown;
end;

destructor TJvSpinButton.Destroy;
begin
  FTopDownBtn.Free;
  FBottomDownBtn.Free;
  FNotDownBtn.Free;
  {$IFDEF JVCLThemesEnabled}
  FTopHotBtn.Free;
  FBottomHotBtn.Free;
  {$ENDIF}
  FUpBitmap.Free;
  FDownBitmap.Free;
  FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TJvSpinButton.DrawAllBitmap;
begin
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
  begin
    {$IFDEF POLESPIN}
    if FButtonStyle = sbsClassic then
      DrawAllBitmapClassicThemed
    else
    {$ENDIF}
      DrawAllBitmapDiagonalThemed;
    Exit;
  end;
  {$ENDIF}

  DrawBitmap(FTopDownBtn, sbTopDown);
  DrawBitmap(FBottomDownBtn, sbBottomDown);
  DrawBitmap(FNotDownBtn, sbNotDown);
end;

{$IFDEF JVCLThemesEnabled}
procedure TJvSpinButton.DrawAllBitmapClassicThemed;
type
  TButtonPartState = (bpsNormal, bpsHot, bpsPressed);
const
  CDetails: array [Boolean, TButtonPartState] of TThemedSpin = (
    (tsUpNormal, tsUpHot, tsUpPressed),
    (tsDownNormal, tsDownHot, tsDownPressed)
    );
var
  TopRect, BottomRect: TRect;
  TopRegion_TopAbove, BottomRegion_TopAbove: HRGN;
  TopRegion_BottomAbove, BottomRegion_BottomAbove: HRGN;

  procedure ConstructThemedButton(ABitmap: TBitmap; const AUpState, ADownState: TButtonPartState);
  var
    Details: TThemedElementDetails;
  begin
    with ABitmap do
    begin
      Height := Self.Height;
      Width := Self.Width;

      with Canvas do
      begin
        { Select only top button }
        if AUpState = bpsNormal then
          SelectClipRgn(Handle, TopRegion_BottomAbove)
        else
          SelectClipRgn(Handle, TopRegion_TopAbove);
        { Copy top button }
        Details := ThemeServices.GetElementDetails(CDetails[False, AUpState]);
        ThemeServices.DrawElement(Handle, Details, TopRect);
        { Select only bottom button }
        if AUpState = bpsNormal then
          SelectClipRgn(Handle, BottomRegion_BottomAbove)
        else
          SelectClipRgn(Handle, BottomRegion_TopAbove);
        { Copy bottom button }
        Details := ThemeServices.GetElementDetails(CDetails[True, ADownState]);
        ThemeServices.DrawElement(Handle, Details, BottomRect);
        { Remove clipping restriction }
        SelectClipRgn(Handle, 0);
      end;
    end;
  end;

begin
  TopRect := Rect(0, 0, Width, Height div 2);
  InflateRect(TopRect, 1, 1);

  BottomRect := Rect(0, TopRect.Bottom, Width, Height);
  InflateRect(BottomRect, 1, 1);

  { Construct the regions (needed because the up & down buttons overlap
    each other) }
  with TopRect do
  begin
    TopRegion_TopAbove := CreateRectRgn(Left, Top, Right, Bottom + 1);
    TopRegion_BottomAbove := CreateRectRgn(Left, Top, Right, Bottom);
  end;
  with BottomRect do
  begin
    BottomRegion_TopAbove := CreateRectRgn(Left, Top + 1, Right, Bottom);
    BottomRegion_BottomAbove := CreateRectRgn(Left, Top, Right, Bottom);
  end;
  try
    { Draw the buttons }
    ConstructThemedButton(FTopDownBtn, bpsPressed, bpsNormal);
    ConstructThemedButton(FBottomDownBtn, bpsNormal, bpsPressed);
    ConstructThemedButton(FNotDownBtn, bpsNormal, bpsNormal);
    ConstructThemedButton(FTopHotBtn, bpsHot, bpsNormal);
    ConstructThemedButton(FBottomHotBtn, bpsNormal, bpsHot);
  finally
    DeleteObject(TopRegion_TopAbove);
    DeleteObject(BottomRegion_TopAbove);
    DeleteObject(TopRegion_BottomAbove);
    DeleteObject(BottomRegion_BottomAbove);
  end;
end;

procedure TJvSpinButton.DrawAllBitmapDiagonalThemed;
type
  TButtonPartState = (bpsNormal, bpsHot, bpsPressed);
const
  CDetails: array [TButtonPartState] of TThemedButton =
    (tbPushButtonNormal, tbPushButtonHot, tbPushButtonPressed);
var
  TemplateButtons: array [TButtonPartState] of TBitmap;
  ThemeColors: array [0..2] of Cardinal;
  ButtonRect: TRect;
  PaintRect: TRect;
  TopRegion, BottomRegion: HRGN;
  UpArrowPos, DownArrowPos: TPoint;
  UpArrowRect, DownArrowRect: TRect;

  procedure ConstructThemedButton(ABitmap: TBitmap; const AUpState, ADownState: TButtonPartState);
  begin
    with ABitmap do
    begin
      Height := Self.Height;
      Width := Self.Width;

      with Canvas do
      begin
        { Select only top button }
        SelectClipRgn(Handle, TopRegion);
        { Copy top button }
        ABitmap.Canvas.Draw(0, 0, TemplateButtons[AUpState]);
        { Select only bottom button }
        SelectClipRgn(Handle, BottomRegion);
        { Copy bottom button }
        ABitmap.Canvas.Draw(0, 0, TemplateButtons[ADownState]);
        { Remove clipping restriction }
        SelectClipRgn(Handle, 0);

        { Draw diagonal }
        Pen.Color := ThemeColors[0];
        MoveTo(PaintRect.Left, PaintRect.Bottom - 2);
        LineTo(PaintRect.Right - 1, PaintRect.Top - 1);

        Pen.Color := ThemeColors[1];
        MoveTo(PaintRect.Right - 1, PaintRect.Top);
        LineTo(PaintRect.Right - 1, PaintRect.Top);
        LineTo(PaintRect.Left, PaintRect.Bottom - 1);

        Pen.Color := ThemeColors[2];
        MoveTo(PaintRect.Left + 1, PaintRect.Bottom - 1);
        LineTo(PaintRect.Right, PaintRect.Top);

        { Draw up arraw }
        with UpArrowPos do
          Draw(X, Y, FUpBitmap);

        { Draw bottom arrow }
        with DownArrowPos do
          Draw(X, Y, FDownBitmap);
      end;
    end;
  end;

var
  ptButton: array [0..2] of TPoint;
  State: TButtonPartState;
  Details: TThemedElementDetails;
begin
  TemplateButtons[bpsNormal] := TBitmap.Create;
  TemplateButtons[bpsHot] := TBitmap.Create;
  TemplateButtons[bpsPressed] := TBitmap.Create;
  try
    ButtonRect := Bounds(0, 0, Width, Height);
    PaintRect := ButtonRect;
    InflateRect(ButtonRect, 1, 1);
    InflateRect(PaintRect, -1, -1);
    { Init templates }
    for State := Low(TButtonPartState) to High(TButtonPartState) do
      with TemplateButtons[State] do
      begin
        Height := Self.Height;
        Width := Self.Width;
        Details := ThemeServices.GetElementDetails(CDetails[State]);
        ThemeServices.DrawElement(Canvas.Handle, Details, ButtonRect);
      end;

    { Init diagonal colors }
    Details := ThemeServices.GetElementDetails(tbPushButtonNormal);
    with Details do
    begin
      GetThemeColor(ThemeServices.Theme[Element], Part, State, TMT_EDGELIGHTCOLOR, ThemeColors[0]);
      GetThemeColor(ThemeServices.Theme[Element], Part, State, TMT_BORDERCOLORHINT, ThemeColors[1]);
      GetThemeColor(ThemeServices.Theme[Element], Part, State, TMT_EDGESHADOWCOLOR, ThemeColors[2]);
    end;

    if FUpBitmap.Handle = 0 then
      FUpBitmap.Handle := LoadBitmap(HInstance, sSpinUpBtn);
    if FDownBitmap.Handle = 0 then
      FDownBitmap.Handle := LoadBitmap(HInstance, sSpinDownBtn);
    try
      FUpBitmap.Transparent := True;
      FDownBitmap.Transparent := True;
      { Init arrow positions }
      UpArrowPos := Point(
        Round((Width / 4) - (FUpBitmap.Width / 2)) + 1,
        Round((Height / 4) - (FUpBitmap.Height / 2)) + 1);
      DownArrowPos := Point(
        Round((3 * Width / 4) - (FDownBitmap.Width / 2)) - 1,
        Round((3 * Height / 4) - (FDownBitmap.Height / 2)) - 1);

      UpArrowRect := Bounds(0, 0, FUpBitmap.Width, FUpBitmap.Height);
      DownArrowRect := Bounds(0, 0, FDownBitmap.Width, FDownBitmap.Height);

      { Init regions, needed to draw the triangles }
      ptButton[0] := Point(ButtonRect.Left, ButtonRect.Bottom);
      ptButton[1] := Point(ButtonRect.Left, ButtonRect.Top);
      ptButton[2] := Point(ButtonRect.Right, ButtonRect.Top);
      TopRegion := CreatePolygonRgn(ptButton, 3, WINDING);
      ptButton[0] := Point(ButtonRect.Right, ButtonRect.Top);
      ptButton[1] := Point(ButtonRect.Right, ButtonRect.Bottom);
      ptButton[2] := Point(ButtonRect.Left, ButtonRect.Bottom);
      BottomRegion := CreatePolygonRgn(ptButton, 3, WINDING);
      try
        { Draw the buttons }
        ConstructThemedButton(FTopDownBtn, bpsPressed, bpsNormal);
        ConstructThemedButton(FBottomDownBtn, bpsNormal, bpsPressed);
        ConstructThemedButton(FNotDownBtn, bpsNormal, bpsNormal);
        ConstructThemedButton(FTopHotBtn, bpsHot, bpsNormal);
        ConstructThemedButton(FBottomHotBtn, bpsNormal, bpsHot);
      finally
        DeleteObject(TopRegion);
        DeleteObject(BottomRegion);
      end;
    finally
      FUpBitmap.Handle := 0;
      FDownBitmap.Handle := 0;
    end;
  finally
    TemplateButtons[bpsNormal].Free;
    TemplateButtons[bpsHot].Free;
    TemplateButtons[bpsPressed].Free;
  end;
end;
{$ENDIF JVCLThemesEnabled}

(*Polaris
procedure TJvSpinButton.DrawBitmap(ABitmap: TBitmap; ADownState: TSpinButtonState);
var
  R, RSrc: TRect;
  dRect: Integer;
  {Temp: TBitmap;}
begin
  ABitmap.Height := Height;
  ABitmap.Width := Width;
  with ABitmap.Canvas do begin
    R := Bounds(0, 0, Width, Height);
    Pen.Width := 1;
    Brush.Color := clBtnFace;
    Brush.Style := bsSolid;
    FillRect(R);
    { buttons frame }
    Pen.Color := clWindowFrame;
    Rectangle(0, 0, Width, Height);
    MoveTo(-1, Height);
    LineTo(Width, -1);
    { top button }
    if ADownState = sbTopDown then Pen.Color := clBtnShadow
    else Pen.Color := clBtnHighlight;
    MoveTo(1, Height - 4);
    LineTo(1, 1);
    LineTo(Width - 3, 1);
    if ADownState = sbTopDown then Pen.Color := clBtnHighlight
      else Pen.Color := clBtnShadow;
    if ADownState <> sbTopDown then begin
      MoveTo(1, Height - 3);
      LineTo(Width - 2, 0);
    end;
    { bottom button }
    if ADownState = sbBottomDown then Pen.Color := clBtnHighlight
      else Pen.Color := clBtnShadow;
    MoveTo(2, Height - 2);
    LineTo(Width - 2, Height - 2);
    LineTo(Width - 2, 1);
    if ADownState = sbBottomDown then Pen.Color := clBtnShadow
      else Pen.Color := clBtnHighlight;
    MoveTo(2, Height - 2);
    LineTo(Width - 1, 1);
    { top glyph }
    dRect := 1;
    if ADownState = sbTopDown then Inc(dRect);
    R := Bounds(Round((Width / 4) - (FUpBitmap.Width / 2)) + dRect,
      Round((Height / 4) - (FUpBitmap.Height / 2)) + dRect, FUpBitmap.Width,
      FUpBitmap.Height);
    RSrc := Bounds(0, 0, FUpBitmap.Width, FUpBitmap.Height);
    {
    if Self.Enabled or (csDesigning in ComponentState) then
      BrushCopy(R, FUpBitmap, RSrc, FUpBitmap.TransparentColor)
    else begin
      Temp := CreateDisabledBitmap(FUpBitmap, clBlack);
      try
        BrushCopy(R, Temp, RSrc, Temp.TransparentColor);
      finally
        Temp.Free;
      end;
    end;
    }
    BrushCopy(R, FUpBitmap, RSrc, FUpBitmap.TransparentColor);
    { bottom glyph }
    R := Bounds(Round((3 * Width / 4) - (FDownBitmap.Width / 2)) - 1,
      Round((3 * Height / 4) - (FDownBitmap.Height / 2)) - 1,
      FDownBitmap.Width, FDownBitmap.Height);
    RSrc := Bounds(0, 0, FDownBitmap.Width, FDownBitmap.Height);
    {
    if Self.Enabled or (csDesigning in ComponentState) then
      BrushCopy(R, FDownBitmap, RSrc, FDownBitmap.TransparentColor)
    else begin
      Temp := CreateDisabledBitmap(FDownBitmap, clBlack);
      try
        BrushCopy(R, Temp, RSrc, Temp.TransparentColor);
      finally
        Temp.Free;
      end;
    end;
    }
    BrushCopy(R, FDownBitmap, RSrc, FDownBitmap.TransparentColor);
    if ADownState = sbBottomDown then begin
      Pen.Color := clBtnShadow;
      MoveTo(3, Height - 2);
      LineTo(Width - 1, 2);
    end;
  end;
end;
*)
type
  TColorArray = array [0..2] of TColor;

procedure TJvSpinButton.DrawBitmap(ABitmap: TBitmap; ADownState: TSpinButtonState);
const
  CColors: TColorArray = (clBtnShadow, clBtnHighlight, clWindowFrame {clBtnFace});
var
  R, RSrc: TRect;
  DRect: Integer;
  Flags: array [0..1] of DWORD;
  LColors: TColorArray;
  LGlyph: array [0..1] of Boolean;
  {Temp: TBitmap;}

  procedure JvDraw;
  begin
    { buttons }
    with ABitmap.Canvas do
    begin
      LColors := CColors;
      if ADownState = sbTopDown then
      begin
        LColors[0] := clBtnFace;
        LColors[2] := clBtnHighlight;
        Flags[0] := EDGE_SUNKEN;
      end;
      if ADownState = sbBottomDown then
      begin
        LColors[1] := clWindowFrame;
        LColors[2] := clBtnShadow;
        Flags[1] := EDGE_SUNKEN;
      end;
      DrawEdge(Handle, R, Flags[0], BF_TOPLEFT or BF_SOFT);
      DrawEdge(Handle, R, Flags[1], BF_BOTTOMRIGHT or BF_SOFT);
      InflateRect(R, -1, -1);

      Pen.Color := LColors[0];
      MoveTo(R.Left, R.Bottom - 2);
      LineTo(R.Right - 1, R.Top - 1);

      Pen.Color := LColors[2];
      MoveTo(R.Right - 1, R.Top);
      LineTo(R.Right - 1, R.Top);
      LineTo(R.Left, R.Bottom - 1);

      Pen.Color := LColors[1];
      MoveTo(R.Left + 1, R.Bottom - 1);
      LineTo(R.Right, R.Top);

      { top glyph }
      DRect := 1;
      if ADownState = sbTopDown then
        Inc(DRect);

      if LGlyph[0] then
        FUpBitmap.Handle := LoadBitmap(HInstance, sSpinUpBtn);
      if LGlyph[1] then
        FDownBitmap.Handle := LoadBitmap(HInstance, sSpinDownBtn);

      R := Bounds(Round((Width / 4) - (FUpBitmap.Width / 2)) + DRect,
        Round((Height / 4) - (FUpBitmap.Height / 2)) + DRect, FUpBitmap.Width,
        FUpBitmap.Height);
      RSrc := Bounds(0, 0, FUpBitmap.Width, FUpBitmap.Height);
      BrushCopy(R, FUpBitmap, RSrc, FUpBitmap.TransparentColor);
      { bottom glyph }
      R := Bounds(Round((3 * Width / 4) - (FDownBitmap.Width / 2)) - 1,
        Round((3 * Height / 4) - (FDownBitmap.Height / 2)) - 1,
        FDownBitmap.Width, FDownBitmap.Height);
      RSrc := Bounds(0, 0, FDownBitmap.Width, FDownBitmap.Height);
      BrushCopy(R, FDownBitmap, RSrc, FDownBitmap.TransparentColor);
      FUpBitmap.Handle := 0;
      FDownBitmap.Handle := 0;
    end;
  end;

  {$IFDEF POLESPIN}
  procedure PoleDraw;
  var
    X, Y, I, J, H: Integer;
    R1: TRect;
  begin
    with ABitmap.Canvas do
    begin
      { top glyph }
      H := Height div 2;
      R := Bounds(0, 0, Width, H);
      if ADownState = sbTopDown then
        Flags[0] := EDGE_SUNKEN
      else
        R.Bottom := R.Bottom + 1;
      if ADownState = sbBottomDown then
        Flags[1] := EDGE_SUNKEN;
      if LGlyph[0] then
        FUpBitmap.Handle := LoadBitmap(HInstance, sSpinUpBtnPole);
      RSrc := R;
      DrawEdge(Handle, R, Flags[0], BF_RECT or BF_SOFT or BF_ADJUST);
      R1 := Bounds(0, H, Width, Height);
      R1.Bottom := Height;
      DrawEdge(Handle, R1, Flags[1], BF_RECT or BF_SOFT or BF_ADJUST);
      I := R.Bottom - R.Top - 1;
      J := R1.Bottom - R1.Top - 1;
      Y := RSrc.Top + (H - FUpBitmap.Height) div 2;
      //      if I >= (J+1) then
      if ADownState = sbTopDown then
        OffsetRect(R1, 0, 1);

      R1.Bottom := R1.Top + I;
      if J - FUpBitmap.Height < 0 then
        Y := R.Top;
      {Glyph}
      FUpBitmap.Transparent := True;
      X := (Width - FUpBitmap.Width) div 2;
      IntersectClipRect(Handle, R.Left, R.Top, R.Right, R.Bottom);
      Draw(X, Y, FUpBitmap);
      SelectClipRgn(Handle, 0);
      RSrc := Bounds(0, H, Width, Height);
      RSrc.Bottom := Height;
      if LGlyph[1] then
        FDownBitmap.Handle := LoadBitmap(HInstance, sSpinDownBtnPole);
      FDownBitmap.Transparent := True;
      X := (Width - FDownBitmap.Width) div 2;
      Y := R1.Top + (I - FDownBitmap.Height) div 2;
      if I - FDownBitmap.Height < 0 then
      begin
        Dec(R1.Top);
        Y := R1.Bottom - FDownBitmap.Height
      end;
      IntersectClipRect(Handle, R1.Left, R1.Top, R1.Right, R1.Bottom);
      Draw(X, Y, FDownBitmap);
      SelectClipRgn(Handle, 0);
    end;
  end;
  {$ENDIF}
begin
  LGlyph[0] := FUpBitmap.Handle = 0;
  LGlyph[1] := FDownBitmap.Handle = 0;
  try
    ABitmap.Height := Height;
    ABitmap.Width := Width;
    FillChar(Flags, SizeOf(Flags), EDGE_RAISED);
    with ABitmap.Canvas do
    begin
      R := Bounds(0, 0, Width, Height);
      Pen.Width := 1;
      Brush.Color := clBtnFace;
      Brush.Style := bsSolid;
      FillRect(R);
    end;
    {$IFDEF POLESPIN}
    if FButtonStyle = sbsClassic then
      PoleDraw
    else
    {$ENDIF}
      JvDraw;
  finally
    if LGlyph[0] then
      FUpBitmap.Handle := 0;
    if LGlyph[1] then
      FDownBitmap.Handle := 0;
  end;
end;

function TJvSpinButton.GetDownGlyph: TBitmap;
begin
  Result := FDownBitmap;
end;

function TJvSpinButton.GetUpGlyph: TBitmap;
begin
  Result := FUpBitmap;
end;

procedure TJvSpinButton.GlyphChanged(Sender: TObject);
begin
  FInvalidate := True;
  Invalidate;
end;

procedure TJvSpinButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    if (FFocusControl <> nil) and FFocusControl.TabStop and
      FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
      FFocusControl.SetFocus;
    if FDown = sbNotDown then
    begin
      FLastDown := FDown;
      //>Polaris
      {$IFNDEF POLESPIN}
      if Y > (-(Height / Width) * X + Height) then
      begin
      {$ELSE}
      if ((FButtonStyle = sbsDefault) and (Y > (-(Height / Width) * X + Height))) or
        ((FButtonStyle = sbsClassic) and (Y > (Height div 2))) then
      begin
      {$ENDIF}
        FDown := sbBottomDown;
        BottomClick;
      end
      else
      begin
        FDown := sbTopDown;
        TopClick;
      end;
      if FLastDown <> FDown then
      begin
        FLastDown := FDown;
        Repaint;
      end;
      if FRepeatTimer = nil then
        FRepeatTimer := TTimer.Create(Self);
      FRepeatTimer.OnTimer := TimerExpired;
      FRepeatTimer.Interval := InitRepeatPause;
      FRepeatTimer.Enabled := True;
    end;
    FDragging := True;
  end;
end;

function TJvSpinButton.MouseInBottomBtn(const P: TPoint): Boolean;
begin
  with P do
    {$IFNDEF POLESPIN}
    Result := Y > (-(Width / Height) * X + Height) then
    {$ELSE}
    Result :=
      ((FButtonStyle = sbsDefault)) and (Y > (-(Width / Height) * X + Height)) or
      ((FButtonStyle = sbsClassic) and (Y > (Height div 2)));
    {$ENDIF}
end;

procedure TJvSpinButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TSpinButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height) then
    begin
      NewState := FDown;
      //>Polaris
      if MouseInBottomBtn(Point(X, Y)) then
      begin
        if FDown <> sbBottomDown then
        begin
          if FLastDown = sbBottomDown then
            FDown := sbBottomDown
          else
            FDown := sbNotDown;
          if NewState <> FDown then
            Repaint;
        end;
      end
      else
      begin
        if FDown <> sbTopDown then
        begin
          if FLastDown = sbTopDown then
            FDown := sbTopDown
          else
            FDown := sbNotDown;
          if NewState <> FDown then
            Repaint;
        end;
      end;
    end
    else
    if FDown <> sbNotDown then
    begin
      FDown := sbNotDown;
      Repaint;
    end;
  end
  {$IFDEF JVCLThemesEnabled}
  else
  if (FMouseInTopBtn or FMouseInBottomBtn) and ThemeServices.ThemesEnabled then
  begin
    if MouseInBottomBtn(Point(X, Y)) then
    begin
      if not FMouseInBottomBtn then
      begin
        FMouseInTopBtn := False;
        FMouseInBottomBtn := True;
        Repaint;
      end;
    end
    else
    begin
      if not FMouseInTopBtn then
      begin
        FMouseInTopBtn := True;
        FMouseInBottomBtn := False;
        Repaint;
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TJvSpinButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    if (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height) then
    begin
      FDown := sbNotDown;
      FLastDown := sbNotDown;
      Repaint;
    end;
  end;
end;

procedure TJvSpinButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TJvSpinButton.Paint;
begin
  if not Enabled and not (csDesigning in ComponentState) then
    FDragging := False;
  if (FNotDownBtn.Height <> Height) or (FNotDownBtn.Width <> Width) or
    FInvalidate then
    DrawAllBitmap;
  FInvalidate := False;
  with Canvas do
    case FDown of
      sbNotDown:
        {$IFDEF JVCLThemesEnabled}
        if FMouseInTopBtn then
          Draw(0, 0, FTopHotBtn)
        else
        if FMouseInBottomBtn then
          Draw(0, 0, FBottomHotBtn)
        else
        {$ENDIF}
          Draw(0, 0, FNotDownBtn);
      sbTopDown:
        Draw(0, 0, FTopDownBtn);
      sbBottomDown:
        Draw(0, 0, FBottomDownBtn);
    end;
end;

//>Polaris

procedure TJvSpinButton.SetButtonStyle(Value: TJvSpinButtonStyle);
begin
  if Value <> FButtonStyle then
  begin
    FButtonStyle := Value;
    GlyphChanged(Self);
  end;
end;
//<Polaris

procedure TJvSpinButton.SetDown(Value: TSpinButtonState);
var
  OldState: TSpinButtonState;
begin
  OldState := FDown;
  FDown := Value;
  if OldState <> FDown then
    Repaint;
end;

procedure TJvSpinButton.SetDownGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FDownBitmap.Assign(Value)
      //  else FDownBitmap.Handle := LoadBitmap(HInstance, sSpinDownBtn);
//Polaris
  else
    FDownBitmap.Handle := 0;
end;

procedure TJvSpinButton.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TJvSpinButton.SetUpGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FUpBitmap.Assign(Value)
      //  else FUpBitmap.Handle := LoadBitmap(HInstance, sSpinUpBtn);
//>Polaris
  else
    FUpBitmap.Handle := 0;
end;

procedure TJvSpinButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FDown <> sbNotDown) and MouseCapture then
  begin
    try
      if FDown = sbBottomDown then
        BottomClick
      else
        TopClick;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TJvSpinButton.TopClick;
begin
  if Assigned(FOnTopClick) then
  begin
    FOnTopClick(Self);
    if not (csLButtonDown in ControlState) then
      FDown := sbNotDown;
  end;
end;

{$IFDEF JVCLThemesEnabled}
procedure TJvSpinButton.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not FMouseInTopBtn and not FMouseInBottomBtn then
  begin
    if MouseInBottomBtn(ScreenToClient(Mouse.CursorPos)) then
      FMouseInBottomBtn := True
    else
      FMouseInTopBtn := True;
    if ThemeServices.ThemesEnabled then
      Repaint;
    inherited MouseEnter(Control);
  end;
end;

procedure TJvSpinButton.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if FMouseInTopBtn or FMouseInBottomBtn then
  begin
    FMouseInTopBtn := False;
    FMouseInBottomBtn := False;
    if ThemeServices.ThemesEnabled then
      Repaint;
    inherited MouseLeave(Control);
  end;
end;
{$ENDIF JVCLThemesEnabled}

//=== TJvUpDown ==============================================================

type
  TJvUpDown = class(TCustomUpDown)
  private
    FChanging: Boolean;
    procedure ScrollMessage(var Msg: TWMVScroll);
    procedure WMHScroll(var Msg: TWMHScroll); message CN_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message CN_VSCROLL;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnClick;
  end;

constructor TJvUpDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Orientation := udVertical;
  Min := -1;
  Max := 1;
  Position := 0;
end;

destructor TJvUpDown.Destroy;
begin
  OnClick := nil;
  inherited Destroy;
end;

procedure TJvUpDown.ScrollMessage(var Msg: TWMVScroll);
begin
  if Msg.ScrollCode = SB_THUMBPOSITION then
  begin
    if not FChanging then
    begin
      FChanging := True;
      try
        if Msg.Pos > 0 then
          Click(btNext)
        else
        if Msg.Pos < 0 then
          Click(btPrev);
        if HandleAllocated then
          SendMessage(Handle, UDM_SETPOS, 0, 0);
      finally
        FChanging := False;
      end;
    end;
  end;
end;

procedure TJvUpDown.WMHScroll(var Msg: TWMHScroll);
begin
  ScrollMessage(TWMVScroll(Msg));
end;

procedure TJvUpDown.WMSize(var Msg: TWMSize);
begin
  inherited;
  if Width <> DefBtnWidth then
    Width := DefBtnWidth;
end;

procedure TJvUpDown.WMVScroll(var Msg: TWMVScroll);
begin
  ScrollMessage(Msg);
end;

//=== TJvCustomSpinEdit ======================================================

procedure TJvCustomSpinEdit.Change;
var
//  OldText: string;
  OldSelStart: Integer;
begin
  { (rb) Maybe move to CMTextChanged }
  if FChanging or not HandleAllocated then
    Exit;

  FChanging := True;
  try
//    OldText := inherited Text;
    OldSelStart := SelStart;
    try
      if not (csDesigning in ComponentState) and (coCheckOnChange in CheckOptions) then
      begin
        CheckValueRange(Value, not (coCropBeyondLimit in CheckOptions)); 
        SetValue(CheckValue(Value));
      end;
    except
      SetValue(CheckValue(Value));
    end;
  finally
    FChanging := False;
  end;
  if FOldValue <> Value then
  begin
    inherited Change;
    FOldValue := Value;
  end;
//  if CompareText(inherited Text, OldText) <> 0 then
//    inherited Change;

  SelStart := OldSelStart;
end;

function TJvCustomSpinEdit.CheckDefaultRange(CheckMax: Boolean): Boolean;
begin
  Result := (FMinValue <> 0) or (FMaxValue <> 0);
end;

function TJvCustomSpinEdit.CheckValue(NewValue: Extended): Extended;
begin
  Result := NewValue;
  {
    if (FMaxValue <> FMinValue) then begin
      if NewValue < FMinValue then
        Result := FMinValue
      else if NewValue > FMaxValue then
        Result := FMaxValue;
    end;
  }
  if FCheckMinValue or FCheckMaxValue then
  begin
    if FCheckMinValue and (NewValue < FMinValue) then
      Result := FMinValue;
    if FCheckMaxValue and (NewValue > FMaxValue) then
      Result := FMaxValue;
  end;
end;

//Polaris
function TJvCustomSpinEdit.CheckValueRange(NewValue: Extended; RaiseOnError: Boolean): Extended;
begin
  Result := CheckValue(NewValue);
  if (FCheckMinValue or FCheckMaxValue) and
    RaiseOnError and (Result <> NewValue) then
    raise ERangeError.CreateFmt(RsEOutOfRangeFloat, [FMinValue, FMaxValue]);
end;

procedure TJvCustomSpinEdit.CMBiDiModeChanged(var Msg: TMessage);
begin
  inherited;
  ResizeButton;
  SetEditRect;
end;

procedure TJvCustomSpinEdit.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  ResizeButton;
  SetEditRect;
end;

procedure TJvCustomSpinEdit.EnabledChanged;
begin
  inherited EnabledChanged;
  if FUpDown <> nil then
  begin
    FUpDown.Enabled := Enabled;
    ResizeButton;
  end;
  if FButton <> nil then
    FButton.Enabled := Enabled;
end;

procedure TJvCustomSpinEdit.DoEnter;
begin
  SetFocused(True);
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited DoEnter;
end;

procedure TJvCustomSpinEdit.DoExit;
begin
  SetFocused(False);
  try
    if not (csDesigning in ComponentState) and (coCheckOnExit in CheckOptions) then
    begin
      CheckValueRange(Value, not (coCropBeyondLimit in CheckOptions));
      SetValue(CheckValue(Value));
    end;
  except
    SetFocused(True);
    SelectAll;
    if CanFocus then
      SetFocus;
    raise;
  end;
  inherited DoExit;
end;

procedure TJvCustomSpinEdit.FontChanged;
begin
  inherited FontChanged;
  ResizeButton;
  SetEditRect;
end;

constructor TJvCustomSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FThousands := False; //new

  //Polaris
  FFocused := False;
  FCheckOptions := [coCheckOnChange, coCheckOnExit, coCropBeyondLimit];
  FLCheckMinValue := True;
  FLCheckMaxValue := True;
  FCheckMinValue := False;
  FCheckMaxValue := False;
  //Polaris
  ControlStyle := ControlStyle - [csSetCaption];
  FIncrement := 1.0;
  FDecimal := 2;
  FEditorEnabled := True;
  FButtonKind := bkDiagonal;
  FArrowKeys := True;
  RecreateButton;
end;

procedure TJvCustomSpinEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array [Boolean, TAlignment] of DWORD =
    ((ES_LEFT, ES_RIGHT, ES_CENTER), (ES_RIGHT, ES_LEFT, ES_CENTER));
begin
  inherited CreateParams(Params);
  // Polaris:
  //    or ES_MULTILINE
  Params.Style := Params.Style or WS_CLIPCHILDREN or
    Alignments[UseRightToLeftAlignment, FAlignment];
end;

procedure TJvCustomSpinEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TJvCustomSpinEdit.DataChanged;
var
  EditFormat: string;
begin
  if (ValueType = vtFloat) and FFocused and (FDisplayFormat <> EmptyStr) then
  begin
    EditFormat := '0';
    if FDecimal > 0 then
      EditFormat := EditFormat + '.' + MakeStr('#', FDecimal);
    EditText := FormatFloat(EditFormat, Value);
  end;
end;

//Polaris

function TJvCustomSpinEdit.DefaultDisplayFormat: string;
begin
  Result := ',0.##';
end;

destructor TJvCustomSpinEdit.Destroy;
begin
  Destroying;
  FChanging := True;
  if FButton <> nil then
  begin
    FButton.Free;
    FButton := nil;
    FBtnWindow.Free;
    FBtnWindow := nil;
  end;
  if FUpDown <> nil then
  begin
    FUpDown.Free;
    FUpDown := nil;
  end;
  inherited Destroy;
end;

procedure TJvCustomSpinEdit.DownClick(Sender: TObject);
var
  OldText: string;
begin
  if ReadOnly then
    MessageBeep(0)
  else
  begin
    FChanging := True;
    try
      OldText := inherited Text;
      Value := Value - FIncrement;
    finally
      FChanging := False;
    end;
    if CompareText(inherited Text, OldText) <> 0 then
    begin
      Modified := True;
      Change;
    end;
    if Assigned(FOnBottomClick) then
      FOnBottomClick(Self);
  end;
end;

{function TJvCustomSpinEdit.TryGetValue(var Value: Extended): Boolean;
var
  S: string;
begin
  try
    S := StringReplace(Text, ThousandSeparator, '', [rfReplaceAll]);
    if ValueType = vtFloat then
      Value := StrToFloat(S)
    else
      if ValueType = vtHex then
        Value := StrToInt('$' + Text)
      else
        Value := StrToInt(S);
    Result := True;
  except
    if ValueType = vtFloat then
      Value := FMinValue
    else
      Value := Trunc(FMinValue);
    Result := False;
  end;
end;}

function TJvCustomSpinEdit.GetAsInteger: Longint;
begin
  Result := Trunc(GetValue);
end;

function TJvCustomSpinEdit.GetButtonKind: TSpinButtonKind;
begin
  if NewStyleControls then
    Result := FButtonKind
  {$IFNDEF POLESPIN}
  else
    Result := bkDiagonal;
  {$ELSE}
    //>Polaris
  else
  begin
    Result := bkDiagonal;
    if Assigned(FButton) and (FButton.ButtonStyle = sbsClassic) then
      Result := bkClassic;
  end;
  //<Polaris
  {$ENDIF}
end;

function TJvCustomSpinEdit.GetButtonWidth: Integer;
begin
  if FUpDown <> nil then
    Result := FUpDown.Width
  else
  if FButton <> nil then
    Result := FButton.Width
  else
    Result := DefBtnWidth;
end;

function TJvCustomSpinEdit.GetMinHeight: Integer;
var
  I, H: Integer;
begin
  GetTextHeight(I, H);
  if I > H then
    I := H;
  Result := H + (GetSystemMetrics(SM_CYBORDER) * 4) + 1;
end;

procedure TJvCustomSpinEdit.GetTextHeight(var SysHeight, Height: Integer);
var
  DC: HDC;
  SaveFont: HFONT;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  SysHeight := SysMetrics.tmHeight;
  Height := Metrics.tmHeight;
end;

function TJvCustomSpinEdit.IsFormatStored: Boolean;
begin
  Result := DisplayFormat <> DefaultDisplayFormat;
end;

function TJvCustomSpinEdit.IsIncrementStored: Boolean;
begin
  Result := FIncrement <> 1.0;
end;

function TJvCustomSpinEdit.IsMaxStored: Boolean;
begin
  Result := MaxValue <> 0.0;
end;

function TJvCustomSpinEdit.IsMinStored: Boolean;
begin
  Result := MinValue <> 0.0;
end;

function TJvCustomSpinEdit.IsValidChar(Key: Char): Boolean;
var
  ValidChars: set of Char;
begin
  ValidChars := DigitChars + ['+', '-'];
  if ValueType = vtFloat then
  begin
    if Pos(DecimalSeparator, Text) = 0 then
    begin
      if not Thousands or (ThousandSeparator <> '.') then
        ValidChars := ValidChars + [DecimalSeparator, '.'] // Polaris
      else
        ValidChars := ValidChars + [DecimalSeparator];
    end;
    if Pos('E', AnsiUpperCase(Text)) = 0 then
      ValidChars := ValidChars + ['e', 'E'];
  end
  else
  if ValueType = vtHex then
  begin
    ValidChars := ValidChars + ['A'..'F', 'a'..'f'];
  end;
  Result := (Key in ValidChars) or (Key < #32);
  if not FEditorEnabled and Result and ((Key >= #32) or
    (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then
    Result := False;
end;

function TJvCustomSpinEdit.IsValueStored: Boolean;
begin
  Result := GetValue <> 0.0;
end;

procedure TJvCustomSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if ArrowKeys and (Key in [VK_UP, VK_DOWN]) then
  begin
    if Key = VK_UP then
      UpClick(Self)
    else
    if Key = VK_DOWN then
      DownClick(Self);
    Key := 0;
  end;
end;

procedure TJvCustomSpinEdit.KeyPress(var Key: Char);
var I: Integer;
begin
 // andreas
  if (Key = DecimalSeparator) and (ValueType = vtFloat) then
  begin
   { If the key is the decimal separator move the caret behind it. }
    I := Pos(DecimalSeparator, Text);
    if I <> 0 then
    begin
      Key := #0;
      SelLength := 0;
      SelStart := I;
      Exit;
    end;
  end;

  if not IsValidChar(Key) then
  begin
    Key := #0;
    MessageBeep(0)
  end;
  //Polaris
  if (Key = '.') and (not Thousands or (ThousandSeparator <> '.')) then
    Key := DecimalSeparator;

  if Key <> #0 then
  begin
    inherited KeyPress(Key);
    if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) then
    begin
      { must catch and remove this, since is actually multi-line }
      GetParentForm(Self).Perform(CM_DIALOGKEY, Byte(Key), 0);
      if Key = Char(VK_RETURN) then
        Key := #0;
    end;
  end;
end;

//Polaris

procedure TJvCustomSpinEdit.Loaded;
begin
  inherited Loaded;
  FLCheckMinValue := True;
  FLCheckMaxValue := True;
  FOldValue := Value;
end;

procedure TJvCustomSpinEdit.RecreateButton;
begin
  if csDestroying in ComponentState then
    Exit;
  FButton.Free;
  FButton := nil;
  FBtnWindow.Free;
  FBtnWindow := nil;
  FUpDown.Free;
  FUpDown := nil;
  if GetButtonKind = bkStandard then
  begin
    FUpDown := TJvUpDown.Create(Self);
    with TJvUpDown(FUpDown) do
    begin
      Visible := True;
      //Polaris
      SetBounds(0, 1, DefBtnWidth, Self.Height);
      if BiDiMode = bdRightToLeft then
        Align := alLeft
      else
        Align := alRight;
      Parent := Self;
      OnClick := UpDownClick;
    end;
  end
  else
  begin
    FBtnWindow := TWinControl.Create(Self);
    FBtnWindow.Visible := True;
    FBtnWindow.Parent := Self;
    {$IFDEF POLESPIN}
    if FButtonKind <> bkClassic then
      FBtnWindow.SetBounds(0, 0, DefBtnWidth, Height)
    else
    {$ENDIF}
      FBtnWindow.SetBounds(0, 0, Height, Height);

    FButton := TJvSpinButton.Create(Self);
    FButton.Visible := True;
    {$IFDEF POLESPIN}
    if FButtonKind = bkClassic then
      FButton.FButtonStyle := sbsClassic;
    {$ENDIF}
    FButton.Parent := FBtnWindow;
    FButton.FocusControl := Self;
    FButton.OnTopClick := UpClick;
    FButton.OnBottomClick := DownClick;
    //Polaris
    FButton.SetBounds(1, 1, FBtnWindow.Width - 1, FBtnWindow.Height - 1);
  end;
end;

procedure TJvCustomSpinEdit.ResizeButton;
var
  R: TRect;
begin
  if FUpDown <> nil then
  begin
    FUpDown.Width := DefBtnWidth;
    if BiDiMode = bdRightToLeft then
      FUpDown.Align := alLeft
    else
      FUpDown.Align := alRight;
  end
  else
    if FButton <> nil then
    begin { bkDiagonal }
      if NewStyleControls and Ctl3D and (BorderStyle = bsSingle) then
        {$IFDEF POLESPIN}
        if FButtonKind = bkClassic then
          R := Bounds(Width - DefBtnWidth - 4, -1, DefBtnWidth, Height - 3)
        else
        {$ENDIF}
          R := Bounds(Width - Height - 1, -1, Height - 3, Height - 3)
      else
        {$IFDEF POLESPIN}
        if FButtonKind = bkClassic then
          R := Bounds(Width - DefBtnWidth, 0, DefBtnWidth, Height)
        else
        {$ENDIF}
          R := Bounds(Width - Height, 0, Height, Height);
      if BiDiMode = bdRightToLeft then
      begin
        if NewStyleControls and Ctl3D and (BorderStyle = bsSingle) then
        begin
          R.Left := -1;
          R.Right := Height - 4;
        end
        else
        begin
          R.Left := 0;
          R.Right := Height;
        end;
      end;
      with R do
        FBtnWindow.SetBounds(Left, Top, Right - Left, Bottom - Top);
      //Polaris
      FButton.SetBounds(1, 1, FBtnWindow.Width - 1, FBtnWindow.Height - 1);
    end;
end;

procedure TJvCustomSpinEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomSpinEdit.SetArrowKeys(Value: Boolean);
begin
  FArrowKeys := Value;
  ResizeButton;
end;

procedure TJvCustomSpinEdit.SetAsInteger(NewValue: Longint);
begin
  SetValue(NewValue);
end;

procedure TJvCustomSpinEdit.SetButtonKind(Value: TSpinButtonKind);
var
  OldKind: TSpinButtonKind;
begin
  OldKind := FButtonKind;
  FButtonKind := Value;
  if OldKind <> GetButtonKind then
  begin
    RecreateButton;
    ResizeButton;
    SetEditRect;
  end;
end;

procedure TJvCustomSpinEdit.SetCheckMaxValue(NewValue: Boolean);
begin
  if FMaxValue <> 0 then
    NewValue := True;
  FCheckMaxValue := NewValue;
  if csLoading in ComponentState then
    FLCheckMaxValue := False;
  SetValue(Value);
end;

procedure TJvCustomSpinEdit.SetCheckMinValue(NewValue: Boolean);
begin
  if FMinValue <> 0 then
    NewValue := True;
  FCheckMinValue := NewValue;
  if csLoading in ComponentState then
    FLCheckMinValue := False;
  SetValue(Value);
end;

procedure TJvCustomSpinEdit.SetDecimal(NewValue: Byte);
begin
  if FDecimal <> NewValue then
  begin
    FDecimal := NewValue;
    Value := GetValue;
  end;
end;

procedure TJvCustomSpinEdit.SetDisplayFormat(const Value: string);
begin
  if DisplayFormat <> Value then
  begin
    FDisplayFormat := Value;
    Invalidate;
  end;
end;

procedure TJvCustomSpinEdit.SetEditRect;
var
  Loc: TRect;
begin
  //Polaris
  if BiDiMode = bdRightToLeft then
  begin
    SetRect(Loc, GetButtonWidth + 1, 0, ClientWidth - 1,
      ClientHeight + 1);
    SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN, MakeLong(GetButtonWidth, 0));
  end
  else
  begin
    SetRect(Loc, 0, 0, ClientWidth - GetButtonWidth - 2, ClientHeight + 1);
    SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN, MakeLong(0, GetButtonWidth));
  end;
  SendMessage(Handle, EM_SETRECTNP, 0, Longint(@Loc));
end;

//Polaris

procedure TJvCustomSpinEdit.SetFocused(Value: Boolean);
begin
  if Value <> FFocused then
  begin
    FFocused := Value;
    Invalidate;
    DataChanged;
  end;
end;

procedure TJvCustomSpinEdit.SetMaxValue(NewValue: Extended);
var
  Z: Boolean;
  B: Boolean;
begin
  if NewValue <> FMaxValue then
  begin
    B := not StoreCheckMaxValue;
    Z := (FMaxValue = 0) <> (NewValue = 0);
    FMaxValue := NewValue;
    if Z and FLCheckMaxValue then
    begin
      SetCheckMaxValue(CheckDefaultRange(True));
      if B and FLCheckMinValue then
        SetCheckMinValue(CheckDefaultRange(False));
    end;
    SetValue(Value);
  end;
end;

procedure TJvCustomSpinEdit.SetMinValue(NewValue: Extended);
var
  Z: Boolean;
  B: Boolean;
begin
  if NewValue <> FMinValue then
  begin
    B := not StoreCheckMinValue;
    Z := (FMinValue = 0) <> (NewValue = 0);
    FMinValue := NewValue;
    if Z and FLCheckMinValue then
    begin
      SetCheckMinValue(CheckDefaultRange(False));
      if B and FLCheckMaxValue then
        SetCheckMaxValue(CheckDefaultRange(True));
    end;
    SetValue(Value);
  end;
end;

procedure TJvCustomSpinEdit.SetThousands(const Value: Boolean);
begin
  if ValueType <> vtHex then
    FThousands := Value;
end;

procedure TJvCustomSpinEdit.SetValueType(NewType: TValueType);
begin
  if FValueType <> NewType then
  begin
    FValueType := NewType;
    Value := GetValue;
    if FValueType in [{$IFDEF BCB} vtInt {$ELSE} vtInteger {$ENDIF}, vtHex] then
    begin
      FIncrement := Round(FIncrement);
      if FIncrement = 0 then
        FIncrement := 1;
    end;
    if FValueType = vtHex then
      Thousands := False;
  end;
end;

function TJvCustomSpinEdit.StoreCheckMaxValue: Boolean;
begin
  Result := (FMaxValue = 0) and (FCheckMaxValue = (FMinValue = 0));
end;

function TJvCustomSpinEdit.StoreCheckMinValue: Boolean;
begin
  Result := (FMinValue = 0) and (FCheckMinValue = (FMaxValue = 0));
end;

procedure TJvCustomSpinEdit.UpClick(Sender: TObject);
var
  OldText: string;
begin
  if ReadOnly then
    MessageBeep(0)
  else
  begin
    FChanging := True;
    try
      OldText := inherited Text;
      Value := Value + FIncrement;
    finally
      FChanging := False;
    end;
    if CompareText(inherited Text, OldText) <> 0 then
    begin
      Modified := True;
      Change;
    end;
    if Assigned(FOnTopClick) then
      FOnTopClick(Self);
  end;
end;

procedure TJvCustomSpinEdit.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  if TabStop and CanFocus then
    SetFocus;
  case Button of
    btNext:
      UpClick(Sender);
    btPrev:
      DownClick(Sender);
  end;
end;

procedure TJvCustomSpinEdit.WMCut(var Msg: TWMCut);
begin
  if not FEditorEnabled or ReadOnly then
    Exit;
  inherited;
end;

procedure TJvCustomSpinEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  if ([coCropBeyondLimit, coCheckOnExit] <= CheckOptions) and not (csDesigning in ComponentState) then
    SetValue(CheckValue(Value));
  inherited;
end;

procedure TJvCustomSpinEdit.WMMouseWheel(var Msg: TWMMouseWheel);
begin
  if Msg.WheelDelta > 0 then
    UpClick(nil)
  else
    DownClick(nil);
end;

procedure TJvCustomSpinEdit.WMPaste(var Msg: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then
    Exit;
  inherited;

  { Polaris code:
  if not FEditorEnabled or ReadOnly then
    Exit;
  V := Value;
  inherited;
  try
    StrToFloat(Text);
  except
    SetValue(V);
  end;
  }
end;

procedure TJvCustomSpinEdit.WMSize(var Msg: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
  { text edit bug: if size to less than minheight, then edit ctrl does
    not display the text }
  if Height < MinHeight then
    Height := MinHeight
  else
  begin
    ResizeButton;
    SetEditRect;
  end;
end;

//=== TJvSpinEdit ============================================================

// (rom) quite unusual not to have it in the Custom base class

constructor TJvSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //{$IFDEF MSWINDOWS}
  //  FButtonKind := bkDiagonal;
  //{$ENDIF}
  Text := '0';
  //  RecreateButton;
end;

function TJvSpinEdit.GetValue: Extended;
begin
  try
    case ValueType of
      vtFloat:
        begin
          if FDisplayFormat <> EmptyStr then
          try
            Result := StrToFloat(TextToValText(Text));
          except
            Result := FMinValue;
          end
          else
          if not TextToFloat(PChar(RemoveThousands(Text)), Result, fvExtended) then
            Result := FMinValue;
        end;
      vtHex:
        Result := StrToIntDef('$' + Text, Round(FMinValue));
    else {vtInteger}
      Result := StrToIntDef(RemoveThousands(Text), Round(FMinValue));
    end;
  except
    if ValueType = vtFloat then
      Result := FMinValue
    else
      Result := Round(FMinValue);
  end;
end;

procedure TJvSpinEdit.SetValue(NewValue: Extended);
var
  FloatFormat: TFloatFormat;
begin
  if Thousands then
    FloatFormat := ffNumber
  else
    FloatFormat := ffFixed;

  case ValueType of
    vtFloat:
      if FDisplayFormat <> EmptyStr then
        Text := FormatFloat(FDisplayFormat, CheckValue(NewValue))
      else
        Text := FloatToStrF(CheckValue(NewValue), FloatFormat, 15, FDecimal);
    vtHex:
      if ValueType = vtHex then
        Text := IntToHex(Round(CheckValue(NewValue)), 1);
  else {vtInteger}
    //Text := IntToStr(Round(CheckValue(NewValue)));
    Text := FloatToStrF(CheckValue(NewValue), FloatFormat, 15, 0);
  end;
  DataChanged;
end;

end.

