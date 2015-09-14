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
  Niels v/d Spek

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:

-----------------------------------------------------------------------------}
// $Id$

unit JvSpin;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  Windows, Messages, CommCtrl, ComCtrls, Controls, ExtCtrls, Graphics, Forms,
  JvExMask, JvComponent, JvDataSourceIntf;

const
  DefaultInitRepeatPause = 400; { pause before repeat timer (ms) }
  DefaultRepeatPause = 100;
  WM_UPDOWNCLICK = WM_APP + $486;

type
  TSpinButtonState = (sbNotDown, sbTopDown, sbBottomDown);

  TJvSpinButtonStyle = (sbsDefault, sbsClassic);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvSpinButton = class(TJvGraphicControl)
  private
    FDown: TSpinButtonState;
    FDragging: Boolean;
    FUpBitmap: TBitmap; // Custom up arrow
    FDownBitmap: TBitmap; // Custom down arrow
    FButtonBitmaps: Pointer;
    {$IFDEF JVCLThemesEnabled}
    FMouseInTopBtn: Boolean;
    FMouseInBottomBtn: Boolean;
    {$ENDIF JVCLThemesEnabled}
    FRepeatTimer: TTimer;
    FLastDown: TSpinButtonState;
    FFocusControl: TWinControl;
    FOnTopClick: TNotifyEvent;
    FOnBottomClick: TNotifyEvent;
    FButtonStyle: TJvSpinButtonStyle;
    FInitRepeatPause: Integer;
    FRepeatPause: Integer;
    procedure SetButtonStyle(Value: TJvSpinButtonStyle);
    procedure TopClick;
    procedure BottomClick;
    procedure GlyphChanged(Sender: TObject);
    function GetDownGlyph: TBitmap;
    function GetUpGlyph: TBitmap;
    procedure SetDown(Value: TSpinButtonState);
    procedure SetDownGlyph(Value: TBitmap);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetUpGlyph(Value: TBitmap);
    procedure TimerExpired(Sender: TObject);
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
  protected
    procedure CheckButtonBitmaps;
    procedure RemoveButtonBitmaps;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

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
    property ButtonStyle: TJvSpinButtonStyle read FButtonStyle write SetButtonStyle default sbsDefault;
    property DragCursor;
    property DragKind;
    property OnEndDock;
    property OnStartDock;
    property DragMode;
    property Enabled;
    property Visible;
    property Height default 20;
    property Width default 20;
    property DownGlyph: TBitmap read GetDownGlyph write SetDownGlyph;
    property UpGlyph: TBitmap read GetUpGlyph write SetUpGlyph;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property InitRepeatPause: Integer read FInitRepeatPause write FInitRepeatPause default DefaultInitRepeatPause;
    property RepeatPause: Integer read FRepeatPause write FRepeatPause default DefaultRepeatPause;
    property ShowHint;
    property ParentShowHint;
    property Anchors;
    property Constraints;
    property OnBottomClick: TNotifyEvent read FOnBottomClick write FOnBottomClick;
    property OnTopClick: TNotifyEvent read FOnTopClick write FOnTopClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
  end;

  {$IFDEF BCB}
  TValueType = (vtInt, vtFloat, vtHex, vtString);
  {$ELSE}
  TValueType = (vtInteger, vtFloat, vtHex, vtString);
  {$ENDIF BCB}

  TSpinButtonKind = (bkStandard, bkDiagonal, bkClassic);

  TJvCheckOption = (coCheckOnChange, coCheckOnExit, coCropBeyondLimit);
  TJvCheckOptions = set of TJvCheckOption;

  TJvCustomSpinEdit = class(TJvExCustomMaskEdit)
  private
    FShowButton: Boolean;
    FCheckMaxValue: Boolean;
    FCheckMinValue: Boolean;
    FCheckOptions: TJvCheckOptions;
    FDisplayFormat: string;
    FFocused: Boolean;
    FLCheckMaxValue: Boolean;
    FLCheckMinValue: Boolean;
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
    FThousands: Boolean; // New
    FIsNegative: Boolean;
    FItems: TStrings;
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
    procedure SetThousands(Value: Boolean);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure SetShowButton(Value: Boolean);
    procedure CMBiDiModeChanged(var Msg: TMessage); message CM_BIDIMODECHANGED;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure WMUpDownClick(var Msg: TMessage); message WM_UPDOWNCLICK;
    procedure SetItems(const AValue: TStrings);
  protected
    FButtonKind: TSpinButtonKind;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure WMCut(var Msg: TMessage); message WM_CUT;
    procedure FocusKilled(NextWnd: THandle); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;  MousePos: TPoint): Boolean; override;
    procedure BoundsChanged; override;
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

    function DefaultDisplayFormat: string; virtual;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat stored IsFormatStored;
    //    procedure DefinePropertyes(Filer: TFiler); override;

    function IsValidChar(Key: Char): Boolean; virtual;
    procedure Change; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DownClick(Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure UpClick(Sender: TObject); virtual;
    property ButtonWidth: Integer read GetButtonWidth;

    procedure DoTopClick;
    procedure DoBottomClick;
  public
    procedure Loaded; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property AsInteger: Longint read GetAsInteger write SetAsInteger default 0;
    property Text;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ArrowKeys: Boolean read FArrowKeys write SetArrowKeys default True;
    property ButtonKind: TSpinButtonKind read FButtonKind write SetButtonKind default bkDiagonal;
    property Decimal: Byte read FDecimal write SetDecimal default 2;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Increment: Extended read FIncrement write FIncrement stored IsIncrementStored;
    property Items: TStrings read FItems write SetItems;
    property MaxValue: Extended read FMaxValue write SetMaxValue stored IsMaxStored;
    property MinValue: Extended read FMinValue write SetMinValue stored IsMinStored;
    property CheckOptions: TJvCheckOptions read FCheckOptions write FCheckOptions default
      [coCheckOnChange, coCheckOnExit, coCropBeyondLimit];
    property CheckMinValue: Boolean read FCheckMinValue write SetCheckMinValue stored StoreCheckMinValue;
    property CheckMaxValue: Boolean read FCheckMaxValue write SetCheckMaxValue stored StoreCheckMaxValue;
    property ValueType: TValueType read FValueType write SetValueType
      default {$IFDEF BCB} vtInt {$ELSE} vtInteger {$ENDIF};
    property Value: Extended read GetValue write SetValue stored IsValueStored;
    property Thousands: Boolean read FThousands write SetThousands default False;
    property ShowButton: Boolean read FShowButton write SetShowButton default True;
    property OnBottomClick: TNotifyEvent read FOnBottomClick write FOnBottomClick;
    property OnTopClick: TNotifyEvent read FOnTopClick write FOnTopClick;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvSpinEdit = class(TJvCustomSpinEdit)
  protected
    procedure SetValue(NewValue: Extended); override;
    function GetValue: Extended; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CheckOptions;
    property CheckMinValue;
    property CheckMaxValue;

    property BeepOnError;

    property Align;
    property Alignment;
    property ArrowKeys;
    property DisplayFormat;
    property ButtonKind default bkDiagonal;
    property Thousands;
    property Decimal;
    property EditorEnabled;
    property Increment;
    property Items;
    property MaxValue;
    property MinValue;
    property ShowButton;
    property ValueType;
    property Value;
    property OnBottomClick;
    property OnTopClick;

    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Color;
    property DragCursor;
    property BiDiMode;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property OnEndDock;
    property OnStartDock;
    property DragMode;
    property Enabled;
    property Font;
    property Anchors;
    property Constraints;
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
    property HideSelection;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property ClipboardCommands;
  end;

  TJvCustomTimeEdit = class;

  TJvCustomTimeEditDataConnector = class(TJvFieldDataConnector)
  private
    FEdit: TJvCustomTimeEdit;
  protected
    procedure RecordChanged; override;
    procedure UpdateData; override;
    property Control: TJvCustomTimeEdit read FEdit;
  public
    constructor Create(AEdit: TJvCustomTimeEdit);
  end;

  TJvCustomTimeEdit = class(TJvCustomSpinEdit)
  private
    Position: Integer;
    FHour24: Boolean;
    FShowSeconds: Boolean;
    FTime: TDateTime;
    FDataConnector: TJvCustomTimeEditDataConnector;
    procedure SetShowSeconds(Value: Boolean);
    procedure SetHour24(Value: Boolean);
    procedure SetDataConnector(const Value: TJvCustomTimeEditDataConnector);
  protected
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure WMCut(var Msg: TMessage); message WM_CUT;
    procedure UpdateTimeDigits(Increment: Boolean);
    function IsTimeValid(const Value: string): Boolean;
    procedure SetValue(NewValue: Extended); override;
    function GetValue: Extended; override;
    function GetTime: TDateTime; virtual;
    procedure SetTime(Value: TDateTime); virtual;

    procedure UpClick(Sender: TObject); Override;
    procedure DownClick(Sender: TObject); Override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    function CreateDataConnector: TJvCustomTimeEditDataConnector; virtual;
    procedure Change; override;
    procedure DoExit; override;

    property ButtonKind default bkDiagonal;
    property ShowSeconds: Boolean read FShowSeconds write SetShowSeconds default False;
    property Hour24: Boolean read FHour24 write SetHour24 default True;
    property DataConnector: TJvCustomTimeEditDataConnector read FDataConnector write SetDataConnector;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Time: TDateTime read GetTime write SetTime;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvTimeEdit = class(TJvCustomTimeEdit)
  published
    property ButtonKind default bkDiagonal;
    property ShowSeconds default False;
    property Hour24 default True;
    property DataConnector;
    property ShowButton;
    property EditorEnabled;
    property ArrowKeys;

    property Align;
    property Alignment;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Color;
    property DragCursor;
    property BiDiMode;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property OnEndDock;
    property OnStartDock;
    property DragMode;
    property Enabled;
    property Font;
    property Anchors;
    property Constraints;
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
    property OnBottomClick;
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
    property OnTopClick;
    property OnContextPopup;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property HideSelection;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property ClipboardCommands;
  end;


{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Types, JvThemes,
  {$IFDEF JVCLThemesEnabled}
  UxTheme,
  {$IFNDEF COMPILER7_UP}
  TmSchema,
  {$ENDIF !COMPILER7_UP}
  {$ENDIF JVCLThemesEnabled}
  JvJCLUtils, JvJVCLUtils, JvConsts, JvResources, JclStrings, JclSysUtils;

{$R JvSpin.Res}

const
  sSpinUpBtn = 'JvSpinUP';
  sSpinDownBtn = 'JvSpinDOWN';
  sSpinUpBtnPole = 'JvSpinUPPOLE';
  sSpinDownBtnPole = 'JvSpinDOWNPOLE';

  sTimeFormats: array [{Hour24}Boolean, {ShowSeconds}Boolean] of string = (
    ('HH:mm AM/PM', 'HH:mm:ss AM/PM'),
    ('HH:mm', 'HH:mm:ss')
  );

type
  TColorArray = array [0..2] of TColor;

  TJvUpDown = class(TCustomUpDown)
  private
    FChanging: Boolean;
    procedure ScrollMessage(var Msg: TWMVScroll);
    procedure WMHScroll(var Msg: TWMHScroll); message CN_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message CN_VSCROLL;
  public
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnClick;
  end;

  { The face of a spin button is stored because they are a bit to complex to
    calculate everytime in a Paint method. There are multiple bitmaps stored
    for a single spin button, eg disable/top-down/bottom down etc.

    The face bitmaps of a spin button are stored in a TSpinButtonBitmaps
    object. Multiple spin buttons can use the same TSpinButtonBitmaps object.
    (That is, identical spin buttons (same height, width, button kind etc.) use the
    same TSpinButtonBitmaps objects) The TSpinButtonBitmaps objects are managed
    by a single TSpinButtonBitmapsManager object.
  }

  TSpinButtonBitmapsManager = class;

  TSpinButtonBitmaps = class(TObject)
  private
    FManager: TSpinButtonBitmapsManager;
    FHeight: Integer;
    FWidth: Integer;
    FStyle: TJvSpinButtonStyle;
    FClientCount: Integer;

    FTopDownBtn: TBitmap;
    FBottomDownBtn: TBitmap;
    FNotDownBtn: TBitmap;
    FDisabledBtn: TBitmap;
    FCustomGlyphs: Boolean;
    FResetOnDraw: Boolean;
    {$IFDEF JVCLThemesEnabled}
    FTopHotBtn: TBitmap;
    FBottomHotBtn: TBitmap;
    FIsThemed: Boolean;
    {$ENDIF JVCLThemesEnabled}
  protected
    procedure DrawAllBitmap;
    procedure DrawBitmap(ABitmap: TBitmap; ADownState: TSpinButtonState; const Enabled: Boolean);
    procedure PoleDrawArrows(ACanvas: TCanvas; const AState: TSpinButtonState; const Enabled: Boolean;
      AUpArrow, ADownArrow: TBitmap);
    procedure JvDrawArrows(ACanvas: TCanvas; const AState: TSpinButtonState; const Enabled: Boolean;
      AUpArrow, ADownArrow: TBitmap);
    {$IFDEF JVCLThemesEnabled}
    procedure DrawAllBitmapClassicThemed;
    procedure DrawAllBitmapDiagonalThemed;
    procedure DrawDiagonalThemedArrows(ACanvas: TCanvas; const AState: TSpinButtonState; const Enabled: Boolean;
      AUpArrow, ADownArrow: TBitmap);
    {$ENDIF JVCLThemesEnabled}
    procedure Reset;

    function CompareWith(const AWidth, AHeight: Integer; const AStyle: TJvSpinButtonStyle;
      const ACustomGlyphs: Boolean): Integer;
  public
    constructor Create(AManager: TSpinButtonBitmapsManager; const AWidth, AHeight: Integer;
      const AStyle: TJvSpinButtonStyle; const ACustomGlyphs: Boolean); virtual;
    destructor Destroy; override;

    procedure AddClient;
    procedure RemoveClient;

    procedure Draw(ACanvas: TCanvas; const ADown: TSpinButtonState;
      const AEnabled, AMouseInTopBtn, AMouseInBottomBtn: Boolean);
    procedure DrawGlyphs(ACanvas: TCanvas; const AState: TSpinButtonState; const Enabled: Boolean;
      AUpArrow, ADownArrow: TBitmap);

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Style: TJvSpinButtonStyle read FStyle;
    property CustomGlyphs: Boolean read FCustomGlyphs;
  end;

  TSpinButtonBitmapsManager = class(TObject)
  private
    FClientCount: Integer;
    FList: TList;
  protected
    function Find(const Width, Height: Integer; const AButtonStyle: TJvSpinButtonStyle;
      const ACustomGlyphs: Boolean; var Index: Integer): Boolean;
    procedure Remove(Obj: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function WantButtons(const Width, Height: Integer; const AButtonStyle: TJvSpinButtonStyle;
      const ACustomGlyphs: Boolean): TSpinButtonBitmaps;

    procedure AddClient;
    procedure RemoveClient;
  end;

var
  GSpinButtonBitmapsManager: TSpinButtonBitmapsManager = nil;

//=== Local procedures =======================================================

function SpinButtonBitmapsManager: TSpinButtonBitmapsManager;
begin
  if GSpinButtonBitmapsManager = nil then
    GSpinButtonBitmapsManager := TSpinButtonBitmapsManager.Create;
  Result := GSpinButtonBitmapsManager;
end;

function DefBtnWidth: Integer;
begin
  Result := GetSystemMetrics(SM_CXVSCROLL);
  if Result > 15 then
    Result := 15;
end;

function RemoveThousands(const AValue: string): string;
begin
  if JclFormatSettings.DecimalSeparator <> JclFormatSettings.ThousandSeparator then
    Result := DelChars(AValue, JclFormatSettings.ThousandSeparator)
  else
    Result := AValue;
end;


//=== { TJvCustomSpinEdit } ==================================================

constructor TJvCustomSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TStringList.Create;
  FThousands := False; //new

  FFocused := False;
  FCheckOptions := [coCheckOnChange, coCheckOnExit, coCropBeyondLimit];
  FLCheckMinValue := True;
  FLCheckMaxValue := True;
  FCheckMinValue := False;
  FCheckMaxValue := False;
  ControlStyle := ControlStyle - [csSetCaption];
  FIncrement := 1.0;
  FDecimal := 2;
  FEditorEnabled := True;
  FButtonKind := bkDiagonal;
  FArrowKeys := True;
  FShowButton := True;
  RecreateButton;
end;

destructor TJvCustomSpinEdit.Destroy;
begin
  Destroying;
  FChanging := True;
  FreeAndNil(FButton);
  FreeAndNil(FBtnWindow);
  FreeAndNil(FUpDown);
  FItems.Free;
  inherited Destroy;
end;

procedure TJvCustomSpinEdit.Change;
var
  OldText: string;
  OldSelStart: Integer;
begin
  { (rb) Maybe move to CMTextChanged }
  if FChanging or not HandleAllocated then
    Exit;

  FChanging := True;
  FIsNegative := False;
  OldSelStart := SelStart;
  try
    OldText := inherited Text;
    if OldText <> '' then
      FIsNegative := Text[1] = '-';
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
    FIsNegative := False; // reset
  end;

  SelStart := OldSelStart;

  if FOldValue <> Value then
  begin
    // Mantis 3469: This has the advantage to be completely transparent to
    // the number of decimals shown in the control

    // (outchy) only shift SelStart by the difference in number of ThousandSeparator BEFORE SelStart
    // do not shift if SelStart was clamped (new text length is shorter than OldSelText)
    if Thousands and (SelStart = OldSelStart) then
      SelStart := SelStart + StrCharCount(Copy(Text, 1, SelStart), JclFormatSettings.ThousandSeparator) -
        StrCharCount(Copy(OldText, 1, SelStart), JclFormatSettings.ThousandSeparator);

    inherited Change;
    FOldValue := Value;
  end;
end;

function TJvCustomSpinEdit.CheckDefaultRange(CheckMax: Boolean): Boolean;
begin
  Result := (FMinValue <> 0) or (FMaxValue <> 0);
end;

function TJvCustomSpinEdit.CheckValue(NewValue: Extended): Extended;
begin
  Result := NewValue;
  {
    if (FMaxValue <> FMinValue) then
    begin
      if NewValue < FMinValue then
        Result := FMinValue
      else
      if NewValue > FMaxValue then
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

function TJvCustomSpinEdit.CheckValueRange(NewValue: Extended; RaiseOnError: Boolean): Extended;
begin
  Result := CheckValue(NewValue);
  if (FCheckMinValue or FCheckMaxValue) and
    RaiseOnError and (Result <> NewValue) then
    raise ERangeError.CreateResFmt(@RsEOutOfRangeFloat, [FMinValue, FMaxValue]);
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

procedure TJvCustomSpinEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array [Boolean, TAlignment] of DWORD =
    ((ES_LEFT, ES_RIGHT, ES_CENTER), (ES_RIGHT, ES_LEFT, ES_CENTER));
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CLIPCHILDREN or Alignments[UseRightToLeftAlignment, FAlignment];
end;

procedure TJvCustomSpinEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TJvCustomSpinEdit.DataChanged;
var
  EditFormat: string;
  WasModified: Boolean;
begin
  if (ValueType = vtFloat) and FFocused and (FDisplayFormat <> '') then
  begin
    EditFormat := '0';
    if FDecimal > 0 then
      EditFormat := EditFormat + '.' + MakeStr('0', FDecimal);   // See Mantis 3936 about the '0' here.
    { Changing EditText sets Modified to false }
    WasModified := Modified;
    try
      Text := FormatFloat(EditFormat, Value);
    finally
      Modified := WasModified;
    end;
  end;
end;

function TJvCustomSpinEdit.DefaultDisplayFormat: string;
begin
  Result := ',0.##';
end;

procedure TJvCustomSpinEdit.BoundsChanged;
var
  MinHeight: Integer;
begin
  MinHeight := GetMinHeight;
  { text edit bug: if size to less than minheight, then edit ctrl does
    not display the text }
  if Height < MinHeight then
    Height := MinHeight
  else
  begin
    ResizeButton;
    SetEditRect;
    inherited BoundsChanged;
  end;
end;

procedure TJvCustomSpinEdit.WMCut(var Msg: TMessage);
begin
  if FEditorEnabled and not ReadOnly then
    inherited;
end;

procedure TJvCustomSpinEdit.WMPaste(var Msg: TMessage);
begin
  if FEditorEnabled and not ReadOnly then
    inherited;
end;

procedure TJvCustomSpinEdit.WMUpDownClick(var Msg: TMessage);
var
  Sender: TObject;
  Button: TUDBtnType;
begin
  Sender := TObject(Msg.WParam);
  Button := TUDBtnType(Msg.LParam);
  if TabStop and CanFocus then
    SetFocus;
  case Button of
    btNext:
      UpClick(Sender);
    btPrev:
      DownClick(Sender);
  end;
end;

procedure TJvCustomSpinEdit.DoBottomClick;
begin
  if Assigned(FOnBottomClick) then
    FOnBottomClick(Self);
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

procedure TJvCustomSpinEdit.FocusKilled(NextWnd: THandle);
begin
  if ([coCropBeyondLimit, coCheckOnExit] <= CheckOptions) and not (csDesigning in ComponentState) then
    SetValue(CheckValue(Value));
  inherited FocusKilled(NextWnd);
end;

function TJvCustomSpinEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  if WheelDelta > 0 then
    UpClick(nil)
  else
    DownClick(nil);
  Result := True;
end;

procedure TJvCustomSpinEdit.DoTopClick;
begin
  if Assigned(FOnTopClick) then
    FOnTopClick(Self);
end;

procedure TJvCustomSpinEdit.DownClick(Sender: TObject);
var
  OldText: string;
begin
  if ReadOnly then
    DoBeepOnError
  else
  begin
    FChanging := True;
    try
      OldText := inherited Text;
      Value := Value - FIncrement;
    finally
      FChanging := False;
    end;
    if AnsiCompareText(inherited Text, OldText) <> 0 then
    begin
      Modified := True;
      Change;
    end;
    DoBottomClick;
  end;
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

procedure TJvCustomSpinEdit.FontChanged;
begin
  inherited FontChanged;
  ResizeButton;
  SetEditRect;
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
  Result := FButtonKind;
end;

function TJvCustomSpinEdit.GetButtonWidth: Integer;
begin
  if ShowButton then
  begin
    if FUpDown <> nil then
      Result := FUpDown.Width
    else
    if FButton <> nil then
      Result := FButton.Width
    else
      Result := DefBtnWidth;
  end
  else
    Result := 0;
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
  DC := GetDC(HWND_DESKTOP);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(HWND_DESKTOP, DC);
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
  ValidChars: TSysCharSet;
begin
  ValidChars := DigitChars + ['+', '-'];
  if ValueType = vtFloat then
  begin
    if Pos(JclFormatSettings.DecimalSeparator, Text) = 0 then
    begin
      if not Thousands or (JclFormatSettings.ThousandSeparator <> '.') then
        ValidChars := ValidChars + [JclFormatSettings.DecimalSeparator, '.']
      else
        ValidChars := ValidChars + [JclFormatSettings.DecimalSeparator];
    end;
    if Pos('E', AnsiUpperCase(Text)) = 0 then
      ValidChars := ValidChars + ['e', 'E'];
  end
  else
  if ValueType = vtHex then
  begin
    ValidChars := ValidChars + ['A'..'F', 'a'..'f'];
  end;
  Result := CharInSet(Key, ValidChars) or (Key < #32);
  if not FEditorEnabled and Result and ((Key >= #32) or
    (Key = BackSpace) or (Key = Del)) then
    Result := False;
end;

function TJvCustomSpinEdit.IsValueStored: Boolean;
begin
  Result := GetValue <> 0.0;
end;

procedure TJvCustomSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if ArrowKeys and ((Key = VK_UP) or (Key = VK_DOWN)) then
  begin
    if Key = VK_UP then
      UpClick(Self)
    else
    if Key = VK_DOWN then
      DownClick(Self);
    Key := 0;
  end;
  // do not delete the decimal separator while typing
  // all decimal digits were moved to the integer part and new decimals were added at the end
  if (Key = VK_DELETE) and (SelStart < Length(Text)) and (Text[SelStart + 1] = JclFormatSettings.DecimalSeparator) then
    Key := VK_RIGHT;
  if (Key = VK_BACK) and (SelStart > 0) and (Text[SelStart] = JclFormatSettings.DecimalSeparator) then
    Key := VK_LEFT;
end;

procedure TJvCustomSpinEdit.KeyPress(var Key: Char);
var
  I: Integer;
begin
  // (outchy) moved at the beginning, hitting '.' now behaves like hitting the decimal separator
  if (Key = '.') and (not Thousands or (JclFormatSettings.ThousandSeparator <> '.')) then
    Key := JclFormatSettings.DecimalSeparator;

  if (Key = JclFormatSettings.DecimalSeparator) and (ValueType = vtFloat) then
  begin
    { If the key is the decimal separator move the caret behind it. }
    I := Pos(JclFormatSettings.DecimalSeparator, Text);
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
    DoBeepOnError;
  end;

  if Key <> #0 then
  begin
    inherited KeyPress(Key);
    if (Key = Cr) or (Key = Esc) then
    begin
      { must catch and remove this, since it is actually multi-line }
      GetParentForm(Self).Perform(CM_DIALOGKEY, Byte(Key), 0);
      if Key = Cr then
        Key := #0;
    end;
  end;
end;

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
  if ShowButton then
    if GetButtonKind = bkStandard then
    begin
      FUpDown := TJvUpDown.Create(Self);
      with TJvUpDown(FUpDown) do
      begin
        Visible := True;
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
      if FButtonKind <> bkClassic then
        FBtnWindow.SetBounds(0, 0, DefBtnWidth, Height)
      else
        FBtnWindow.SetBounds(0, 0, Height, Height);
      FButton := TJvSpinButton.Create(Self);
      FButton.Visible := True;
      if FButtonKind = bkClassic then
        FButton.FButtonStyle := sbsClassic;
      FButton.Parent := FBtnWindow;
      FButton.FocusControl := Self;
      FButton.OnTopClick := UpClick;
      FButton.OnBottomClick := DownClick;
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
    if Ctl3D and (BorderStyle = bsSingle) then
      if FButtonKind = bkClassic then
        R := Bounds(Width - DefBtnWidth - 4, -1, DefBtnWidth, Height - 3)
      else
        R := Bounds(Width - Height - 1, -1, Height - 3, Height - 3)
    else
      if FButtonKind = bkClassic then
      R := Bounds(Width - DefBtnWidth, 0, DefBtnWidth, Height)
    else
      R := Bounds(Width - Height, 0, Height, Height);
    if BiDiMode = bdRightToLeft then
    begin
      if Ctl3D and (BorderStyle = bsSingle) then
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
    FBtnWindow.SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
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

procedure TJvCustomSpinEdit.SetShowButton(Value: Boolean);
begin
  if FShowButton <> Value then
  begin
    FShowButton := Value;
    RecreateButton;
    ResizeButton;
    SetEditRect;
  end;
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
  if BiDiMode = bdRightToLeft then
  begin
    SetRect(Loc, GetButtonWidth + 1, 0, ClientWidth - 1, ClientHeight + 1);
    SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN, MakeLong(GetButtonWidth, 0));
  end
  else
  begin
    SetRect(Loc, 0, 0, ClientWidth - GetButtonWidth - 2, ClientHeight + 1);
    SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN, MakeLong(0, GetButtonWidth));
  end;
  SendMessage(Handle, EM_SETRECTNP, 0, LPARAM(@Loc));
end;

procedure TJvCustomSpinEdit.SetFocused(Value: Boolean);
begin
  if Value <> FFocused then
  begin
    FFocused := Value;
    Invalidate;
    DataChanged;
  end;
end;

procedure TJvCustomSpinEdit.SetItems(const AValue: TStrings);
begin
  FItems.Assign(AValue);
  Value := 0;
  Text := FItems[0];
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

procedure TJvCustomSpinEdit.SetThousands(Value: Boolean);
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
    DoBeepOnError
  else
  begin
    FChanging := True;
    try
      OldText := inherited Text;
      Value := Value + FIncrement;
    finally
      FChanging := False;
    end;
    if AnsiCompareText(inherited Text, OldText) <> 0 then
    begin
      Modified := True;
      Change;
    end;
    DoTopClick;
  end;
end;

procedure TJvCustomSpinEdit.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  PostMessage(Handle, WM_UPDOWNCLICK, WPARAM(Sender), LPARAM(Button));
end;

//=== { TJvSpinButton } ======================================================

constructor TJvSpinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonStyle := sbsDefault;
  FUpBitmap := TBitmap.Create;
  FDownBitmap := TBitmap.Create;
  FUpBitmap.OnChange := GlyphChanged;
  FDownBitmap.OnChange := GlyphChanged;
  Height := 20;
  Width := 20;
  FLastDown := sbNotDown;
  FButtonBitmaps := nil;
  FInitRepeatPause := DefaultInitRepeatPause;
  FRepeatPause := DefaultRepeatPause;

  SpinButtonBitmapsManager.AddClient;
end;

destructor TJvSpinButton.Destroy;
begin
  RemoveButtonBitmaps;
  SpinButtonBitmapsManager.RemoveClient;

  FUpBitmap.Free;
  FDownBitmap.Free;
  FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TJvSpinButton.BottomClick;
begin
  if Assigned(FOnBottomClick) then
  begin
    FOnBottomClick(Self);
    if not (csLButtonDown in ControlState) then
      FDown := sbNotDown;
  end;
end;

procedure TJvSpinButton.CheckButtonBitmaps;
begin
  if Assigned(FButtonBitmaps) and
    ((TSpinButtonBitmaps(FButtonBitmaps).Height <> Height) or
     (TSpinButtonBitmaps(FButtonBitmaps).Width <> Width)) then
    RemoveButtonBitmaps;

  if FButtonBitmaps = nil then
  begin
    FButtonBitmaps := SpinButtonBitmapsManager.WantButtons(Width, Height, ButtonStyle,
      not FUpBitmap.Empty or not FDownBitmap.Empty);
    TSpinButtonBitmaps(FButtonBitmaps).AddClient;
  end;
end;

procedure TJvSpinButton.CMSysColorChange(var Msg: TMessage);
begin
  // The buttons we draw are buffered, thus we need to repaint them to theme changes etc.
  if FButtonBitmaps <> nil then
    TSpinButtonBitmaps(FButtonBitmaps).Reset;
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
  if Sender is TBitmap then
    TBitmap(Sender).Transparent := True;
  RemoveButtonBitmaps;
  Invalidate;
end;

procedure TJvSpinButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
      if ((FButtonStyle = sbsDefault) and (Y > (-(Height / Width) * X + Height))) or
        ((FButtonStyle = sbsClassic) and (Y > (Height div 2))) then
      begin
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

{$IFDEF JVCLThemesEnabled}
procedure TJvSpinButton.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  { (rb) only themed spin buttons have hot states, so it's not necessary
         to calc FMouseInBottomBtn and FMouseInTopBtn for non-themed apps }
  if not FMouseInTopBtn and not FMouseInBottomBtn then
  begin
    if MouseInBottomBtn(ScreenToClient(Mouse.CursorPos)) then
      FMouseInBottomBtn := True
    else
      FMouseInTopBtn := True;
    if StyleServices.Enabled then
      Repaint;
    inherited MouseEnter(Control);
  end;
end;
{$ENDIF JVCLThemesEnabled}

function TJvSpinButton.MouseInBottomBtn(const P: TPoint): Boolean;
begin
  Result :=
    ((FButtonStyle = sbsDefault)) and (P.Y > (-(Width / Height) * P.X + Height)) or
    ((FButtonStyle = sbsClassic) and (P.Y > (Height div 2)));
end;

{$IFDEF JVCLThemesEnabled}
procedure TJvSpinButton.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if FMouseInTopBtn or FMouseInBottomBtn then
  begin
    FMouseInTopBtn := False;
    FMouseInBottomBtn := False;
    if StyleServices.Enabled then
      Repaint;
    inherited MouseLeave(Control);
  end;
end;
{$ENDIF JVCLThemesEnabled}

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
  if (FMouseInTopBtn or FMouseInBottomBtn) and StyleServices.Enabled then
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
  {$ENDIF JVCLThemesEnabled}
end;

procedure TJvSpinButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TJvSpinButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TJvSpinButton.Paint;
begin
  CheckButtonBitmaps;

  if not Enabled and not (csDesigning in ComponentState) then
    FDragging := False;

  {$IFDEF JVCLThemesEnabled}
  TSpinButtonBitmaps(FButtonBitmaps).Draw(Canvas, FDown, Enabled, FMouseInTopBtn, FMouseInBottomBtn);
  {$ELSE}
  TSpinButtonBitmaps(FButtonBitmaps).Draw(Canvas, FDown, Enabled, False, False);
  {$ENDIF JVCLThemesEnabled}
  if not FUpBitmap.Empty or not FDownBitmap.Empty then
    TSpinButtonBitmaps(FButtonBitmaps).DrawGlyphs(Canvas, FDown, Enabled, FUpBitmap, FDownBitmap);
end;

procedure TJvSpinButton.RemoveButtonBitmaps;
begin
  if Assigned(FButtonBitmaps) then
  begin
    TSpinButtonBitmaps(FButtonBitmaps).RemoveClient;
    FButtonBitmaps := nil;
  end;
end;

procedure TJvSpinButton.SetButtonStyle(Value: TJvSpinButtonStyle);
begin
  if Value <> FButtonStyle then
  begin
    FButtonStyle := Value;
    GlyphChanged(Self);
  end;
end;

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
  else
    FDownBitmap.Handle := NullHandle;
end;

procedure TJvSpinButton.SetFocusControl(Value: TWinControl);
begin
  ReplaceComponentReference(Self, Value, TComponent(FFocusControl));
end;

procedure TJvSpinButton.SetUpGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FUpBitmap.Assign(Value)
  else
    FUpBitmap.Handle := NullHandle;
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

//=== { TJvSpinEdit } ========================================================

// (rom) quite unusual not to have it in the Custom base class

constructor TJvSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text := '0';
end;

function TJvSpinEdit.GetValue: Extended;
begin
  try
    case ValueType of
      vtFloat:
        begin
          if FDisplayFormat <> '' then
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
      vtString:
        begin
          Result := FItems.IndexOf(Text);
          if Result < 0 then
            Result := 0;
        end
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
  WasModified: Boolean;
begin
  if Thousands then
    FloatFormat := ffNumber
  else
    FloatFormat := ffFixed;

  { Changing EditText sets Modified to false }
  WasModified := Modified;
  try
    case ValueType of
      vtFloat:
        if FDisplayFormat <> '' then
          Text := FormatFloat(FDisplayFormat, CheckValue(NewValue))
        else
          Text := FloatToStrF(CheckValue(NewValue), FloatFormat, 15, FDecimal);
      vtHex:
        if ValueType = vtHex then
          Text := IntToHex(Round(CheckValue(NewValue)), 1);
      vtString:
      begin
        NewValue := CheckValue(NewValue);
        if NewValue < 0 then
          NewValue := 0;
        if NewValue >= Items.Count then
          NewValue := Items.Count - 1;
        Text := Items[Trunc(NewValue)];
      end
    else {vtInteger}
      //Text := IntToStr(Round(CheckValue(NewValue)));
      Text := FloatToStrF(CheckValue(NewValue), FloatFormat, 15, 0);
    end;
    if FIsNegative and (Text <> '') and (Text[1] <> '-') then
      Text := '-' + Text;
    DataChanged;
  finally
    Modified := WasModified;
  end;
end;

//=== { TJvUpDown } ==========================================================

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

procedure TJvUpDown.Resize;
begin
  if Width <> DefBtnWidth then
    Width := DefBtnWidth
  else
    inherited Resize;
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

procedure TJvUpDown.WMVScroll(var Msg: TWMVScroll);
begin
  ScrollMessage(Msg);
end;

//=== { TSpinButtonBitmaps } =================================================

constructor TSpinButtonBitmaps.Create(AManager: TSpinButtonBitmapsManager;
  const AWidth, AHeight: Integer; const AStyle: TJvSpinButtonStyle; const ACustomGlyphs: Boolean);
begin
  inherited Create;
  FManager := AManager;
  FWidth := AWidth;
  FHeight := AHeight;
  FStyle := AStyle;
  FCustomGlyphs := ACustomGlyphs;

  FTopDownBtn := TBitmap.Create;
  FBottomDownBtn := TBitmap.Create;
  FNotDownBtn := TBitmap.Create;
  FDisabledBtn := TBitmap.Create;
  {$IFDEF JVCLThemesEnabled}
  FTopHotBtn := TBitmap.Create;
  FBottomHotBtn := TBitmap.Create;
  {$ENDIF JVCLThemesEnabled}

  DrawAllBitmap;
end;

destructor TSpinButtonBitmaps.Destroy;
begin
  FManager.Remove(Self);

  FTopDownBtn.Free;
  FBottomDownBtn.Free;
  FNotDownBtn.Free;
  FDisabledBtn.Free;
  {$IFDEF JVCLThemesEnabled}
  FTopHotBtn.Free;
  FBottomHotBtn.Free;
  {$ENDIF JVCLThemesEnabled}

  inherited Destroy;
end;

procedure TSpinButtonBitmaps.AddClient;
begin
  Inc(FClientCount);
end;

function TSpinButtonBitmaps.CompareWith(const AWidth, AHeight: Integer;
  const AStyle: TJvSpinButtonStyle; const ACustomGlyphs: Boolean): Integer;
begin
  // used by the binary search
  Result := Self.Width - AWidth;
  if Result = 0 then
  begin
    Result := Self.Height - AHeight;
    if Result = 0 then
    begin
      Result := Ord(Self.Style) - Ord(AStyle);
      if Result = 0 then
        Result := Ord(Self.CustomGlyphs) - Ord(ACustomGlyphs);
    end;
  end;
end;

procedure TSpinButtonBitmaps.Draw(ACanvas: TCanvas;
  const ADown: TSpinButtonState; const AEnabled, AMouseInTopBtn, AMouseInBottomBtn: Boolean);
begin
  if FResetOnDraw then
  begin
    DrawAllBitmap;
    FResetOnDraw := False;
  end;

  with ACanvas do
    if not AEnabled then
      Draw(0, 0, FDisabledBtn)
    else
      case ADown of
        sbNotDown:
          {$IFDEF JVCLThemesEnabled}
          if FIsThemed then
          begin
            if AMouseInTopBtn then
              Draw(0, 0, FTopHotBtn)
            else
            if AMouseInBottomBtn then
              Draw(0, 0, FBottomHotBtn)
            else
              Draw(0, 0, FNotDownBtn);
          end
          else
          {$ENDIF JVCLThemesEnabled}
            Draw(0, 0, FNotDownBtn);
        sbTopDown:
          Draw(0, 0, FTopDownBtn);
        sbBottomDown:
          Draw(0, 0, FBottomDownBtn);
      end;
end;

procedure TSpinButtonBitmaps.DrawAllBitmap;
begin
  {$IFDEF JVCLThemesEnabled}
  FIsThemed := StyleServices.Enabled;
  if FIsThemed then
  begin
    if FStyle = sbsClassic then
      DrawAllBitmapClassicThemed
    else
      DrawAllBitmapDiagonalThemed;
    Exit;
  end;
  {$ENDIF JVCLThemesEnabled}

  DrawBitmap(FTopDownBtn, sbTopDown, True);
  DrawBitmap(FBottomDownBtn, sbBottomDown, True);
  DrawBitmap(FNotDownBtn, sbNotDown, True);
  DrawBitmap(FDisabledBtn, sbNotDown, False);
end;

{$IFDEF JVCLThemesEnabled}

procedure TSpinButtonBitmaps.DrawAllBitmapClassicThemed;
type
  TButtonPartState = (bpsNormal, bpsHot, bpsPressed, bpsDisabled);
const
  CDetails: array [Boolean, TButtonPartState] of TThemedSpin = (
    (tsUpNormal, tsUpHot, tsUpPressed, tsUpDisabled),
    (tsDownNormal, tsDownHot, tsDownPressed, tsDownDisabled)
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
        // Select only top button
        if AUpState = bpsNormal then
          SelectClipRgn(Handle, TopRegion_BottomAbove)
        else
          SelectClipRgn(Handle, TopRegion_TopAbove);
        // Copy top button
        Details := StyleServices.GetElementDetails(CDetails[False, AUpState]);
        StyleServices.DrawElement(Handle, Details, TopRect);
        // Select only bottom button
        if AUpState = bpsNormal then
          SelectClipRgn(Handle, BottomRegion_BottomAbove)
        else
          SelectClipRgn(Handle, BottomRegion_TopAbove);
        // Copy bottom button
        Details := StyleServices.GetElementDetails(CDetails[True, ADownState]);
        StyleServices.DrawElement(Handle, Details, BottomRect);
        // Remove clipping restriction
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
  TopRegion_TopAbove := CreateRectRgn(TopRect.Left, TopRect.Top, TopRect.Right, TopRect.Bottom + 1);
  TopRegion_BottomAbove := CreateRectRgn(TopRect.Left, TopRect.Top, TopRect.Right, TopRect.Bottom);
  BottomRegion_TopAbove := CreateRectRgn(BottomRect.Left, BottomRect.Top + 1, BottomRect.Right, BottomRect.Bottom);
  BottomRegion_BottomAbove := CreateRectRgn(BottomRect.Left, BottomRect.Top, BottomRect.Right, BottomRect.Bottom);
  try
    { Draw the buttons }
    ConstructThemedButton(FTopDownBtn, bpsPressed, bpsNormal);
    ConstructThemedButton(FBottomDownBtn, bpsNormal, bpsPressed);
    ConstructThemedButton(FNotDownBtn, bpsNormal, bpsNormal);
    ConstructThemedButton(FTopHotBtn, bpsHot, bpsNormal);
    ConstructThemedButton(FBottomHotBtn, bpsNormal, bpsHot);
    ConstructThemedButton(FDisabledBtn, bpsDisabled, bpsDisabled);
  finally
    DeleteObject(TopRegion_TopAbove);
    DeleteObject(BottomRegion_TopAbove);
    DeleteObject(TopRegion_BottomAbove);
    DeleteObject(BottomRegion_BottomAbove);
  end;
end;

procedure TSpinButtonBitmaps.DrawAllBitmapDiagonalThemed;
type
  TButtonPartState = (bpsNormal, bpsHot, bpsPressed, bpsDisabled);
const
  CDetails: array [TButtonPartState] of TThemedButton =
    (tbPushButtonNormal, tbPushButtonHot, tbPushButtonPressed, tbPushButtonDisabled);
var
  I: TButtonPartState;
  TemplateButtons: array [TButtonPartState] of TBitmap;
  ThemeColors: array [0..2] of Cardinal;
  ButtonRect: TRect;
  PaintRect: TRect;
  TopRegion, BottomRegion: HRGN;
  UpBitmap, DownBitmap: TBitmap;
  ptButton: array [0..2] of TPoint;
  State: TButtonPartState;
  Details: TThemedElementDetails;

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

        if not CustomGlyphs then
          DrawDiagonalThemedArrows(ABitmap.Canvas, sbNotDown,
            AUpState <> bpsDisabled, UpBitmap, DownBitmap);
      end;
    end;
  end;

begin
  for I := Low(TemplateButtons) to High(TemplateButtons) do
    TemplateButtons[I] := TBitmap.Create;
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
        Details := StyleServices.GetElementDetails(CDetails[State]);
        StyleServices.DrawElement(Canvas.Handle, Details, ButtonRect);
      end;

    { Init diagonal colors }
    Details := StyleServices.GetElementDetails(tbPushButtonNormal);
    with Details do
    begin
      GetThemeColor(StyleServices.Theme[Element], Part, State, TMT_EDGELIGHTCOLOR, ThemeColors[0]);
      GetThemeColor(StyleServices.Theme[Element], Part, State, TMT_BORDERCOLORHINT, ThemeColors[1]);
      GetThemeColor(StyleServices.Theme[Element], Part, State, TMT_EDGESHADOWCOLOR, ThemeColors[2]);
    end;

    UpBitmap := nil;
    DownBitmap := nil;
    try
      if not CustomGlyphs then
      begin
        UpBitmap := TBitmap.Create;
        UpBitmap.Handle := LoadBitmap(HInstance, sSpinUpBtn);
        UpBitmap.Transparent := True;
        DownBitmap := TBitmap.Create;
        DownBitmap.Handle := LoadBitmap(HInstance, sSpinDownBtn);
        DownBitmap.Transparent := True;
      end;

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
        ConstructThemedButton(FDisabledBtn, bpsDisabled, bpsDisabled);
      finally
        DeleteObject(TopRegion);
        DeleteObject(BottomRegion);
      end;
    finally
      UpBitmap.Free;
      DownBitmap.Free;
    end;
  finally
    for I := Low(TemplateButtons) to High(TemplateButtons) do
      TemplateButtons[I].Free;
  end;
end;

{$ENDIF JVCLThemesEnabled}

procedure TSpinButtonBitmaps.DrawBitmap(ABitmap: TBitmap; ADownState: TSpinButtonState; const Enabled: Boolean);
const
  CColors: TColorArray = (clBtnShadow, clBtnHighlight, cl3DDkShadow);
var
  ButtonRect: TRect;
  LColors: TColorArray;
  UpArrow, DownArrow: TBitmap;

  procedure JvDraw;
  var
    TopFlags, BottomFlags: DWORD;
    R: TRect;
  begin
    TopFlags := EDGE_RAISED;
    BottomFlags := EDGE_RAISED;
    R := ButtonRect;

    with ABitmap.Canvas do
    begin
      LColors := CColors;
      if ADownState = sbTopDown then
      begin
        LColors[0] := clBtnFace;
        LColors[2] := clBtnHighlight;
        TopFlags := EDGE_SUNKEN;
      end;
      if ADownState = sbBottomDown then
      begin
        LColors[1] := clWindowFrame;
        LColors[2] := clBtnShadow;
        BottomFlags := EDGE_SUNKEN;
      end;
      DrawEdge(Handle, R, TopFlags, BF_TOPLEFT or BF_SOFT);
      DrawEdge(Handle, R, BottomFlags, BF_BOTTOMRIGHT or BF_SOFT);
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

      if not CustomGlyphs then
      begin
        UpArrow.Assign(nil); // fixes GDI resource leak
        UpArrow.LoadFromResourceName(HInstance, sSpinUpBtn);
        UpArrow.TransparentColor := clWhite;
        UpArrow.Transparent := True;
        DownArrow.Assign(nil); // fixes GDI resource leak
        DownArrow.LoadFromResourceName(HInstance, sSpinDownBtn);
        DownArrow.TransparentColor := clWhite;
        DownArrow.Transparent := True;
        JvDrawArrows(ABitmap.Canvas, ADownState, Enabled, UpArrow, DownArrow);
      end;
    end;
  end;

  procedure PoleDraw;
  var
    H: Integer;
    TopFlags, BottomFlags: DWORD;
    R, R1: TRect;
    RSrc: TRect;
  begin
    TopFlags := EDGE_RAISED;
    BottomFlags := EDGE_RAISED;

    with ABitmap.Canvas do
    begin
      { top glyph }
      H := Height div 2;
      R := Bounds(0, 0, Width, H);
      if ADownState = sbTopDown then
        TopFlags := EDGE_SUNKEN
      else
        R.Bottom := R.Bottom + 1;
      if ADownState = sbBottomDown then
        BottomFlags := EDGE_SUNKEN;
      RSrc := R;
      DrawEdge(Handle, R, TopFlags, BF_RECT or BF_SOFT or BF_ADJUST);
      R1 := Bounds(0, H, Width, Height);
      R1.Bottom := Height;
      DrawEdge(Handle, R1, BottomFlags, BF_RECT or BF_SOFT or BF_ADJUST);
      if not CustomGlyphs then
      begin
        UpArrow.Assign(nil); // fixes GDI resource leak
        UpArrow.LoadFromResourceName(HInstance, sSpinUpBtnPole);
        UpArrow.TransparentColor := clWhite;
        UpArrow.Transparent := True;
        DownArrow.Assign(nil); // fixes GDI resource leak
        DownArrow.LoadFromResourceName(HInstance, sSpinDownBtnPole);
        DownArrow.TransparentColor := clWhite;
        DownArrow.Transparent := True;
        PoleDrawArrows(ABitmap.Canvas, ADownState, Enabled, UpArrow, DownArrow);
      end;
    end;
  end;

begin
  UpArrow := nil;
  DownArrow := nil;
  try
    if not CustomGlyphs then
    begin
      UpArrow := TBitmap.Create;
      DownArrow := TBitmap.Create;
    end;

    ABitmap.Height := Height;
    ABitmap.Width := Width;

    with ABitmap.Canvas do
    begin
      ButtonRect := Bounds(0, 0, Width, Height);
      Pen.Width := 1;
      Brush.Color := clBtnFace;
      Brush.Style := bsSolid;
      FillRect(ButtonRect);
    end;
    if FStyle = sbsClassic then
      PoleDraw
    else
      JvDraw;
  finally
    UpArrow.Free;
    DownArrow.Free;
  end;
end;

{$IFDEF JVCLThemesEnabled}

procedure TSpinButtonBitmaps.DrawDiagonalThemedArrows(ACanvas: TCanvas; const AState: TSpinButtonState;
  const Enabled: Boolean; AUpArrow, ADownArrow: TBitmap);
var
  UpArrowPos, DownArrowPos: TPoint;
//  UpArrowRect, DownArrowRect: TRect;
  DisabledBitmap: TBitmap;
begin
  { Init arrow positions }
  UpArrowPos := Point(
    Round((Width / 4) - (AUpArrow.Width / 2)) + 1,
    Round((Height / 4) - (AUpArrow.Height / 2)) + 1);
  DownArrowPos := Point(
    Round((3 * Width / 4) - (ADownArrow.Width / 2)) - 1,
    Round((3 * Height / 4) - (ADownArrow.Height / 2)) - 1);

  //UpArrowRect := Bounds(0, 0, AUpArrow.Width, AUpArrow.Height);
  //DownArrowRect := Bounds(0, 0, ADownArrow.Width, ADownArrow.Height);

  with ACanvas do
  begin
    { Draw up arraw }
    if Enabled then
      Draw(UpArrowPos.X, UpArrowPos.Y, AUpArrow)
    else
    begin
      DisabledBitmap := CreateDisabledBitmap(AUpArrow, clBlack);
      try
        Draw(UpArrowPos.X, UpArrowPos.Y, DisabledBitmap);
      finally
        DisabledBitmap.Free;
      end;
    end;

    { Draw bottom arrow }
    if Enabled then
      Draw(DownArrowPos.X, DownArrowPos.Y, ADownArrow)
    else
    begin
      DisabledBitmap := CreateDisabledBitmap(ADownArrow, clBlack);
      try
        Draw(DownArrowPos.X, DownArrowPos.Y, DisabledBitmap);
      finally
        DisabledBitmap.Free;
      end;
    end;
  end;
end;

{$ENDIF JVCLThemesEnabled}

procedure TSpinButtonBitmaps.DrawGlyphs(ACanvas: TCanvas; const AState: TSpinButtonState; const Enabled: Boolean;
  AUpArrow, ADownArrow: TBitmap);
begin
  {$IFDEF JVCLThemesEnabled}
  if FIsThemed then
  begin
    if FStyle <> sbsClassic then
      DrawDiagonalThemedArrows(ACanvas, AState, Enabled, AUpArrow, ADownArrow);
    Exit;
  end;
  {$ENDIF JVCLThemesEnabled}
  if FStyle = sbsClassic then
    PoleDrawArrows(ACanvas, AState, Enabled, AUpArrow, ADownArrow)
  else
    JvDrawArrows(ACanvas, AState, Enabled, AUpArrow, ADownArrow)
end;

procedure TSpinButtonBitmaps.JvDrawArrows(ACanvas: TCanvas; const AState: TSpinButtonState;
  const Enabled: Boolean; AUpArrow, ADownArrow: TBitmap);
var
  Dest, Source: TRect;
  DeltaRect: Integer;
  DisabledBitmap: TBitmap;
begin
  { buttons }
  with ACanvas do
  begin
    { top glyph }
    DeltaRect := 1;
    if AState = sbTopDown then
      Inc(DeltaRect);

    Dest := Bounds(Round((Width / 4) - (AUpArrow.Width / 2)) + DeltaRect,
      Round((Height / 4) - (AUpArrow.Height / 2)) + DeltaRect, AUpArrow.Width,
      AUpArrow.Height);
    Source := Bounds(0, 0, AUpArrow.Width, AUpArrow.Height);

    if Enabled then
      BrushCopy( Dest, AUpArrow, Source, AUpArrow.TransparentColor)
    else
    begin
      DisabledBitmap := CreateDisabledBitmap(AUpArrow, clBlack);
      try
        BrushCopy( Dest, DisabledBitmap, Source, DisabledBitmap.TransparentColor);
      finally
        DisabledBitmap.Free;
      end;
    end;

    { bottom glyph }
    Dest := Bounds(Round((3 * Width / 4) - (ADownArrow.Width / 2)) - 1,
      Round((3 * Height / 4) - (ADownArrow.Height / 2)) - 1,
      ADownArrow.Width, ADownArrow.Height);
    Source := Bounds(0, 0, ADownArrow.Width, ADownArrow.Height);

    if Enabled then
      BrushCopy( Dest, ADownArrow, Source, ADownArrow.TransparentColor)
    else
    begin
      DisabledBitmap := CreateDisabledBitmap(ADownArrow, clBlack);
      try
        BrushCopy( Dest, DisabledBitmap, Source, DisabledBitmap.TransparentColor);
      finally
        DisabledBitmap.Free;
      end;
    end;
  end;
end;

procedure TSpinButtonBitmaps.PoleDrawArrows(ACanvas: TCanvas;
  const AState: TSpinButtonState; const Enabled: Boolean; AUpArrow, ADownArrow: TBitmap);
var
  X, Y, I, J, H: Integer;
  R1: TRect;
  R: TRect;
  DisabledBitmap: TBitmap;
begin
  with ACanvas do
  begin
    H := Height div 2;
    R := Bounds(0, 0, Width, H);
    if AState <> sbTopDown then
      R.Bottom := R.Bottom + 1;
    R1 := Bounds(0, H, Width, Height);
    R1.Bottom := Height;
    I := R.Bottom - R.Top - 1;
    J := R1.Bottom - R1.Top - 1;
    Y := R.Top + (H - AUpArrow.Height) div 2;
    if AState = sbTopDown then
      OffsetRect(R1, 0, 1);

    R1.Bottom := R1.Top + I;
    if J - AUpArrow.Height < 0 then
      Y := R.Top;
    X := (Width - AUpArrow.Width) div 2;

    IntersectClipRect(Handle, R.Left, R.Top, R.Right, R.Bottom);
    if Enabled then
      Draw(X, Y, AUpArrow)
    else
    begin
      DisabledBitmap := CreateDisabledBitmap(AUpArrow, clBlack);
      try
        Draw(X, Y, DisabledBitmap);
      finally
        DisabledBitmap.Free;
      end;
    end;
    SelectClipRgn(Handle, 0);

    X := (Width - ADownArrow.Width) div 2;
    Y := R1.Top + (I - ADownArrow.Height) div 2;
    if I - ADownArrow.Height < 0 then
    begin
      Dec(R1.Top);
      Y := R1.Bottom - ADownArrow.Height
    end;

    IntersectClipRect(Handle, R1.Left, R1.Top, R1.Right, R1.Bottom);
    if Enabled then
      Draw(X, Y, ADownArrow)
    else
    begin
      DisabledBitmap := CreateDisabledBitmap(ADownArrow, clBlack);
      try
        Draw(X, Y, DisabledBitmap);
      finally
        DisabledBitmap.Free;
      end;
    end;
    SelectClipRgn(Handle, 0);
  end;
end;

procedure TSpinButtonBitmaps.RemoveClient;
begin
  Dec(FClientCount);
  if FClientCount = 0 then
    Self.Free;
end;

procedure TSpinButtonBitmaps.Reset;
begin
  FResetOnDraw := True;
end;

//=== { TSpinButtonBitmapsManager } ==========================================

constructor TSpinButtonBitmapsManager.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TSpinButtonBitmapsManager.Destroy;
begin
  while FList.Count > 0 do
    // this will implicitly remove the object from the list
    TObject(FList[0]).Free;

  FList.Free;
  inherited Destroy;
end;

procedure TSpinButtonBitmapsManager.AddClient;
begin
  Inc(FClientCount);
end;

function TSpinButtonBitmapsManager.Find(const Width, Height: Integer;
  const AButtonStyle: TJvSpinButtonStyle; const ACustomGlyphs: Boolean;
  var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  // same binary search as Classes.TStringList.Find
  Result := False;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := TSpinButtonBitmaps(FList[I]).CompareWith(Width, Height, AButtonStyle, ACustomGlyphs);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

procedure TSpinButtonBitmapsManager.Remove(Obj: TObject);
begin
  FList.Remove(Obj);
end;

procedure TSpinButtonBitmapsManager.RemoveClient;
begin
  Dec(FClientCount);
  if FClientCount = 0 then
  begin
    if Self = GSpinButtonBitmapsManager then
      GSpinButtonBitmapsManager := nil;
    Self.Free;
  end;
end;

function TSpinButtonBitmapsManager.WantButtons(const Width, Height: Integer;
  const AButtonStyle: TJvSpinButtonStyle; const ACustomGlyphs: Boolean): TSpinButtonBitmaps;
var
  Index: Integer;
begin
  if not Find(Width, Height, AButtonStyle, ACustomGlyphs, Index) then
    FList.Insert(Index, TSpinButtonBitmaps.Create(Self, Width, Height, AButtonStyle, ACustomGlyphs));
  Result := TSpinButtonBitmaps(FList[Index]);
end;

//=== { TCustomTimeEdit } ==========================================

procedure TJvCustomTimeEdit.Change;
begin
  DataConnector.Modify;
  inherited Change;
end;

constructor TJvCustomTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption] + [csReplicatable];
  FDataConnector := CreateDataConnector;
  FHour24 := True;
  Time := Now; // updates Text
end;

function TJvCustomTimeEdit.CreateDataConnector: TJvCustomTimeEditDataConnector;
begin
  Result := TJvCustomTimeEditDataConnector.Create(Self);
end;

procedure TJvCustomTimeEdit.SetDataConnector(const Value: TJvCustomTimeEditDataConnector);
begin
  if Value <> FDataConnector then
    FDataConnector.Assign(Value);
end;

function TJvCustomTimeEdit.GetTime: TDateTime;
begin
  Result := 0.0;
  if (Text <> '') and IsTimeValid(Text) then
    Result := Int(fTime) + StrToTime(Text);
end;

procedure TJvCustomTimeEdit.SetTime(Value: TDateTime);
begin
  if FTime <> Value then
  begin
    FTime := Value;
    Text := FormatDateTime(sTimeFormats[Hour24, ShowSeconds], FTime);
  end;
end;

function TJvCustomTimeEdit.IsTimeValid(const Value: string): Boolean;
var
  dt: TDateTime;
begin
  Result := TryStrToTime(Value, dt);
end;

procedure TJvCustomTimeEdit.SetHour24(Value: Boolean);
begin
  if Value <> FHour24 then
  begin
    FHour24 := Value;
    Text := FormatDateTime(sTimeFormats[Hour24, ShowSeconds], Time);
  end;
end;

procedure TJvCustomTimeEdit.SetShowSeconds(Value: Boolean);
begin
  if Value <> FShowSeconds then
  begin
    FShowSeconds := Value;
    Text := FormatDateTime(sTimeFormats[Hour24, ShowSeconds], Time);
  end;
end;

procedure TJvCustomTimeEdit.UpdateTimeDigits(Increment: Boolean);

  procedure SetNumberChar(var S: string; APos: Integer; AValue: Char);
  begin
    S[APos] := AValue;
  end;

  procedure IncNumberChar(var S: string; APos: Integer);
  begin
    S[APos] := Succ(S[APos]);
  end;

  procedure DecNumberChar(var S: string; APos: Integer);
  begin
    S[APos] := Pred(S[APos]);
  end;

var
  Offset, AMPMOffset: Integer;
  NewValue: string;
begin
  if ReadOnly then
  begin
    MessageBeep(0);
    Exit;
  end;
  if Text = '' then
    Exit;

  NewValue := Text;

  AMPMOffset := 10;
  if not FShowSeconds then
    AMPMOffset := 7;

  Position := SelStart;
  // Hours
  if (SelStart = 0) or (SelStart = 1) or (SelStart = 2) then
  begin
    if Hour24 then
    begin
      if Increment then
      begin
        if (NewValue[1] = '2') and (NewValue[2] = '3') then
        begin
          SetNumberChar(NewValue, 1, '0');
          SetNumberChar(NewValue, 2, '0');
        end
        else
        if NewValue[2] = '9' then
        begin
          SetNumberChar(NewValue, 2, '0');
          IncNumberChar(NewValue, 1);
        end
        else
          IncNumberChar(NewValue, 2);
      end
      else // decrement
      begin
        if (NewValue[1] = '0') and (NewValue[2] = '0') then
        begin
          SetNumberChar(NewValue, 1, '2');
          SetNumberChar(NewValue, 2, '3');
        end
        else
        if NewValue[2] = '0' then
        begin
          DecNumberChar(NewValue, 1);
          SetNumberChar(NewValue, 2, '9');
        end
        else
          DecNumberChar(NewValue, 2);
      end;
    end

    else // Hour 12 AM/PM
    begin
      if Increment then
      begin
        if (NewValue[1] = '1') and (NewValue[2] = '2') then
        begin
          SetNumberChar(NewValue, 1, '0');
          SetNumberChar(NewValue, 2, '1');
          if NewValue[AMPMOffset] = 'A' then
            SetNumberChar(NewValue, AMPMOffset, 'P')
          else
            SetNumberChar(NewValue, AMPMOffset, 'A');
        end
        else
        if NewValue[2] = '9' then
        begin
          IncNumberChar(NewValue, 1);
          SetNumberChar(NewValue, 2, '0');
        end
        else
          IncNumberChar(NewValue, 2);
      end
      else // decrement
      begin
        if (NewValue[1] = '0') and (NewValue[2] = '1') then
        begin
          SetNumberChar(NewValue, 1, '1');
          SetNumberChar(NewValue, 2, '2');
          if NewValue[AMPMOffset] = 'A' then
            SetNumberChar(NewValue, AMPMOffset, 'P')
          else
            SetNumberChar(NewValue, AMPMOffset, 'A');
        end
        else
        if NewValue[2] = '0' then
        begin
          SetNumberChar(NewValue, 1, '0');
          SetNumberChar(NewValue, 2, '9');
        end
        else
          DecNumberChar(NewValue, 2);
      end;
    end;
  end

  // Minutes
  else
  if (SelStart >= 3) and (SelStart <= AMPMOffset - 2) then
  begin
    Offset := 7;
    if (SelStart <= 5) then
      Offset := 4;

    if Increment then
    begin
      if (NewValue[Offset] = '5') and (NewValue[Offset + 1] = '9') then
      begin
        SetNumberChar(NewValue, Offset, '0');
        SetNumberChar(NewValue, Offset + 1, '0');
      end
      else
      if NewValue[Offset + 1] = '9' then
      begin
        IncNumberChar(NewValue, Offset);
        SetNumberChar(NewValue, Offset + 1, '0');
      end
      else
        IncNumberChar(NewValue, Offset + 1);
    end
    else // decrement
    begin
      if (NewValue[Offset] = '0') and (NewValue[Offset + 1] = '0') then
      begin
        SetNumberChar(NewValue, Offset, '5');
        SetNumberChar(NewValue, Offset + 1, '9');
      end
      else
      if NewValue[Offset + 1] = '0' then
      begin
        DecNumberChar(NewValue, Offset);
        SetNumberChar(NewValue, Offset + 1, '9');
      end
      else
        DecNumberChar(NewValue, Offset + 1);
    end;
  end

  // AM/PM
  else
  if not Hour24 and (SelStart >= AMPMOffset - 1) and (SelStart <= AMPMOffset + 2) then
  begin
    case NewValue[AMPMOffset] of
      'A': NewValue[AMPMOffset] := 'P';
      'a': NewValue[AMPMOffset] := 'a';
      'P': NewValue[AMPMOffset] := 'A';
      'p': NewValue[AMPMOffset] := 'a';
    end;
  end;
  Text := NewValue;
  SelStart := Position;
end;

procedure TJvCustomTimeEdit.WMCut(var Msg: TMessage);
begin
  if EditorEnabled and not ReadOnly then
    DataConnector.Edit;
  inherited;
end;

procedure TJvCustomTimeEdit.WMPaste(var Msg: TMessage);
begin
  if EditorEnabled and not ReadOnly then
    DataConnector.Edit;
  inherited;
end;

procedure TJvCustomTimeEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := LResult(DataConnector.GetDataLink);
end;

procedure TJvCustomTimeEdit.UpClick(Sender: TObject);
begin
  if ReadOnly then
    DoBeepOnError
  else
  begin
    UpdateTimeDigits(True);
    DoTopClick;
  end;
end;

destructor TJvCustomTimeEdit.Destroy;
begin
  FreeAndNil(FDataConnector);
  inherited Destroy;
end;

procedure TJvCustomTimeEdit.DoExit;
begin
  try
    DataConnector.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  inherited DoExit;
end;

procedure TJvCustomTimeEdit.DownClick(Sender: TObject);
begin
  if ReadOnly then
    DoBeepOnError
  else
  begin
    UpdateTimeDigits(False);
    DoBottomClick;
  end;
end;

procedure TJvCustomTimeEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    DataConnector.Edit;

  inherited KeyDown(Key, Shift);
end;

procedure TJvCustomTimeEdit.KeyPress(var Key: Char);

  function SetNumberChar(const S: string; APos: Integer; AValue: Char): string;
  begin
    Result := S;
    Result[APos] := AValue;
  end;

var
  TimePos, MaxLen: Integer;
begin
  MaxLen := 5; // '00:00'
  if ShowSeconds then
    Inc(MaxLen, 3); // ':00'
  if not Hour24 then
    Inc(MaxLen, 3); // ' AM'

  case SelStart of
    0: if not CharInSet(Key, ['0'..'2']) then Key := #0;
    1: if not CharInSet(Key, ['0'..'9']) then Key := #0;
    2: if CharInSet(Key, ['0'..'5']) then // allow the user to skip the ':'
         SelStart := SelStart + 1
       else
         Key := ':';
    3: if not CharInSet(Key, ['0'..'5']) then Key := #0;
    4: if not CharInSet(Key, ['0'..'9']) then Key := #0;
  end;
  if SelStart >= 5 then
  begin
    TimePos := SelStart;
    if not FShowSeconds then
      Inc(TimePos, 3);
    if SelStart < MaxLen then
    begin
      case TimePos of
        5: if CharInSet(Key, ['0'..'5']) then
             SelStart := SelStart + 1 // allow the user to skip the ':'
           else
             Key := ':';
        6: if not CharInSet(Key, ['0'..'5']) then Key := #0;
        7: if not CharInSet(Key, ['0'..'9']) then Key := #0;
        8: Key := ' ';
        9: if (Key = 'a') or (Key = 'A') then Key := 'A'
           else if (Key = 'p') or (Key = 'P') then Key := 'P'
           else Key := #0;
       10: if (Key = 'm') or (Key = 'M') then Key := 'M'
           else Key := #0;
      end;
    end
    else
      Key := #0;
  end;

  if (SelStart <> Length(Text)) and (Key <> #0) then
  begin
    Position := SelStart;
    Text := SetNumberChar(Text, SelStart + 1, Key);
    SelStart := Position + 1;
    Key := #0;
  end;

  if Length(Text) > MaxLen then
    Key := #0;
end;

function TJvCustomTimeEdit.GetValue: Extended;
begin
  Result := Time;
end;

procedure TJvCustomTimeEdit.SetValue(NewValue: Extended);
begin
  Time := NewValue;
end;

//=== { TJvCustomTimeEditDataConnector } ====================================

constructor TJvCustomTimeEditDataConnector.Create(AEdit: TJvCustomTimeEdit);
begin
  inherited Create;
  FEdit := AEdit;
end;

procedure TJvCustomTimeEditDataConnector.RecordChanged;
begin
  if Field.IsValid then
  begin
    FEdit.ReadOnly := not Field.CanModify;
    if Field.IsNull then
    begin
      FEdit.Time := 0.0;
      FEdit.Text := ''
    end
    else
    begin
      // if field was null before (empty text), then force the edit to update
      // its Text property by forcing a change to its Time property
      if FEdit.Text = '' then
        FEdit.Time := FEdit.Time + 1;

      FEdit.Time := Field.AsDateTime;
    end;
  end
  else
  begin
    FEdit.Time := 0.0;
    FEdit.Text := '';
    FEdit.ReadOnly := True;
  end;
end;

procedure TJvCustomTimeEditDataConnector.UpdateData;
begin
  if FEdit.Text = '' then
    Field.Clear
  else
    Field.AsDateTime := FEdit.Time;
  RecordChanged; // update to stored value
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
