{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ColorCtrls.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit ColorCtrls;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics,
  ComCtrls, StdCtrls, ExtCtrls,
  ColorSpaces, ColorRotate,
  JvTypes;

type
  TJvColorAxisConfig = (acXYZ, acXZY, acYXZ, acYZX, acZXY, acZYX);
  TJvColorOrientation = 0..1;
  TJvArrowPosition = 0..1;

const
  coLeftToRight = TJvColorOrientation(0);
  coRightToLeft = TJvColorOrientation(1);
  coTopToBottom = TJvColorOrientation(0);
  coBottomToTop = TJvColorOrientation(1);

  apLeft = TJvArrowPosition(0);
  apRight = TJvArrowPosition(1);
  apTop = TJvArrowPosition(0);
  apBottom = TJvArrowPosition(1);

type
  TKeyCode = (kcLeft, kcRight, kcUp, kcDown);

  TJvMouseColorEvent = procedure(Sender: TObject; ColorX, ColorY: Byte) of object;
  TJvColorComponent = class;
  TJvColorPanel = class;
  TJvColorCircle = class;
  TJvFullColorTrackBar = class;

  TJvColorComponent = class(TCustomControl)
  private
    FAutoMouse: Boolean;
    FFullColor: TJvFullColor;
    FAxisConfig: TJvColorAxisConfig;
    FOnColorChange: TNotifyEvent;
    FOnAxisConfigChange: TNotifyEvent;
    FOnColorSpaceChange: TNotifyEvent;
    FOnMouseColor: TJvMouseColorEvent;
    FColorChanging: Boolean;
    FBuffer: TBitmap;
    FCreating: Boolean;
    FWantDrawBuffer: Boolean;
    function GetColorSpace: TJvColorSpace;
    procedure SetAxisConfig(const Value: TJvColorAxisConfig);
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMColorChanged(var Msg: TMessage); message CM_COLORCHANGED;
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure SetWantDrawBuffer(Value: Boolean);
  protected
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawFocus;
    procedure DrawFrame(X, Y: Integer);
    procedure SetFullColor(const Value: TJvFullColor); virtual;
    procedure MouseColor(Shift: TShiftState; X, Y: Integer); virtual;
    procedure AxisConfigChange; virtual;
    procedure DrawBuffer; virtual;
    procedure ColorSpaceChange; virtual;
    procedure CalcSize; virtual;
    procedure KeyMove(KeyCode: TKeyCode; MoveCount: Integer); virtual;
    property WantDrawBuffer: Boolean read FWantDrawBuffer write SetWantDrawBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ColorSpace: TJvColorSpace read GetColorSpace;
  published
    property AutoMouse: Boolean read FAutoMouse write FAutoMouse default True;
    property FullColor: TJvFullColor read FFullColor write SetFullColor;
    property AxisConfig: TJvColorAxisConfig read FAxisConfig write SetAxisConfig default acXYZ;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property Color;
    property ParentColor;
    property TabOrder;
    property TabStop default True;
    property OnColorChange: TNotifyEvent read FOnColorChange write FOnColorChange;
    property OnAxisConfigChange: TNotifyEvent read FOnAxisConfigChange write FOnAxisConfigChange;
    property OnColorSpaceChange: TNotifyEvent read FOnColorSpaceChange write FOnColorSpaceChange;
    property OnMouseColor: TJvMouseColorEvent read FOnMouseColor write FOnMouseColor;
  end;

  TJvColorComponent2D = class(TJvColorComponent)
  private
    FValueZAuto: Boolean;
    FValueZ: Byte;
    FAxisConfigChanging: Boolean;
    procedure SetValueZ(const Value: Byte);
    procedure SetValueZAuto(const Value: Boolean);
    procedure UpdateDefaultValueZ;
    function IsValueZStored: Boolean;
  protected
    procedure AxisConfigChange; override;
    procedure ColorSpaceChange; override;
    procedure TrackBarColorChange(Sender: TObject); virtual;
    procedure TrackBarAxisConfigChange(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ValueZAuto: Boolean read FValueZAuto write SetValueZAuto stored False;
    property ValueZ: Byte read FValueZ write SetValueZ stored IsValueZStored default 0;
  end;

  TJvColorPanel = class(TJvColorComponent2D)
  private
    FReverseAxisY: Boolean;
    FReverseAxisX: Boolean;
    FCrossSize: Integer;
    FPen: TPen;
    FCrossCenter: Integer;
    FColorTrackBar: TJvFullColorTrackBar;
    FAxisConfigChanging: Boolean;
    procedure SetReverseAxisX(const Value: Boolean);
    procedure SetReverseAxisY(const Value: Boolean);
    procedure SetCrossSize(Value: Integer);
    procedure SetCrossCenter(Value: Integer);
    procedure SetPen(const Value: TPen);
    procedure SetColorTrackBar(const Value: TJvFullColorTrackBar);
  protected
    procedure PenChange(Sender: TObject);
    procedure MouseColor(Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetFullColor(const Value: TJvFullColor); override;
    procedure DrawBuffer; override;
    procedure CalcSize; override;
    procedure AxisConfigChange; override;
    procedure KeyMove(KeyCode: TKeyCode; MoveCount: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ReverseAxisX: Boolean read FReverseAxisX write SetReverseAxisX default False;
    property ReverseAxisY: Boolean read FReverseAxisY write SetReverseAxisY default False;
    property CrossSize: Integer read FCrossSize write SetCrossSize default 5;
    property CrossCenter: Integer read FCrossCenter write SetCrossCenter default 1;
    property CrossStyle: TPen read FPen write SetPen;
    property ColorTrackBar: TJvFullColorTrackBar read FColorTrackBar write SetColorTrackBar;
  end;

  TJvColorCircleStyle = (crShowLines, crShowCommon, crShowRed, crShowGreen, crShowBlue,
    cr3ButtonsMouse, cr3ButtonsCommon);

  TJvColorCircleStyles = set of TJvColorCircleStyle;

  TJvColorCircle = class(TJvColorComponent2D)
  private
    FStyles: TJvColorCircleStyles;
    FGreenColor: TJvFullColor;
    FBlueColor: TJvFullColor;
    FRedColor: TJvFullColor;
    FInvertRotation: Boolean;
    FInvertRadius: Boolean;
    FCrossCenter: Integer;
    FCrossSize: Integer;
    FCrossStyle: TPen;
    FLineWidth: Integer;
    FDraggingColor: TJvRotateColor;
    FOnRedColorChange: TNotifyEvent;
    FOnBlueColorChange: TNotifyEvent;
    FOnGreenColorChange: TNotifyEvent;
    FOnColorSpaceChange: TNotifyEvent;
    FBlueColorTrackBar: TJvFullColorTrackBar;
    FGreenColorTrackBar: TJvFullColorTrackBar;
    FRedColorTrackBar: TJvFullColorTrackBar;
    FCommonColorTrackBar: TJvFullColorTrackBar;
    FCrossRedColor: TColor;
    FCrossBlueColor: TColor;
    FCrossGreenColor: TColor;
    procedure SetBlueColor(const Value: TJvFullColor);
    procedure SetGreenColor(const Value: TJvFullColor);
    procedure SetRedColor(const Value: TJvFullColor);
    procedure SetStyles(const Value: TJvColorCircleStyles);
    procedure SetInvertRadius(const Value: Boolean);
    procedure SetInvertRotation(const Value: Boolean);
    procedure SetCrossCenter(Value: Integer);
    procedure SetCrossSize(Value: Integer);
    procedure SetCrossStyle(const Value: TPen);
    procedure SetLineWidth(Value: Integer);
    procedure SetBlueColorTrackBar(const Value: TJvFullColorTrackBar);
    procedure SetGreenColorTrackBar(const Value: TJvFullColorTrackBar);
    procedure SetRedColorTrackBar(const Value: TJvFullColorTrackBar);
    procedure SetCommonColorTrackBar(const Value: TJvFullColorTrackBar);
    procedure SetCrossBlueColor(const Value: TColor);
    procedure SetCrossGreenColor(const Value: TColor);
    procedure SetCrossRedColor(const Value: TColor);
  protected
    procedure AxisConfigChange; override;
    procedure ColorSpaceChange; override;
    procedure Paint; override;
    procedure PenChanged(Sender: TObject);
    procedure DrawBuffer; override;
    procedure CalcSize; override;
    procedure SetFullColor(const Value: TJvFullColor); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseColor(Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure TrackBarColorChange(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ConvertToID(NewFullColor: TJvFullColor);
    function FullColorToPosition(AFullColor: TJvFullColor): TPoint;
    function PositionToFullColor(APoint: TPoint): TJvFullColor;
  published
    property Color;
    property ParentColor;
    property InvertRadius: Boolean read FInvertRadius write SetInvertRadius default False;
    property InvertRotation: Boolean read FInvertRotation write SetInvertRotation default False;
    property RedColor: TJvFullColor read FRedColor write SetRedColor default fclRGBRed;
    property GreenColor: TJvFullColor read FGreenColor write SetGreenColor default fclRGBLime;
    property BlueColor: TJvFullColor read FBlueColor write SetBlueColor default fclRGBBlue;
    // (rom) set default value
    property Styles: TJvColorCircleStyles read FStyles write SetStyles;
    property CrossSize: Integer read FCrossSize write SetCrossSize default 5;
    property CrossCenter: Integer read FCrossCenter write SetCrossCenter default 1;
    property CrossStyle: TPen read FCrossStyle write SetCrossStyle;
    property CrossRedColor: TColor read FCrossRedColor write SetCrossRedColor default clMaroon;
    property CrossGreenColor: TColor read FCrossGreenColor write SetCrossGreenColor default clGreen;
    property CrossBlueColor: TColor read FCrossBlueColor write SetCrossBlueColor default clNavy;
    property LineWidth: Integer read FLineWidth write SetLineWidth default 1;
    property RedTrackBar: TJvFullColorTrackBar read FRedColorTrackBar write SetRedColorTrackBar;
    property GreenTrackBar: TJvFullColorTrackBar read FGreenColorTrackBar write SetGreenColorTrackBar;
    property BlueTrackBar: TJvFullColorTrackBar read FBlueColorTrackBar write SetBlueColorTrackBar;
    property CommonTrackBar: TJvFullColorTrackBar read FCommonColorTrackBar write SetCommonColorTrackBar;
    property OnRedColorChange: TNotifyEvent read FOnRedColorChange write FOnRedColorChange;
    property OnGreenColorChange: TNotifyEvent read FOnGreenColorChange write FOnGreenColorChange;
    property OnBlueColorChange: TNotifyEvent read FOnBlueColorChange write FOnBlueColorChange;
    property OnColorSpaceChange: TNotifyEvent read FOnColorSpaceChange write FOnColorSpaceChange;
  end;

  TJvFullColorTrackBar = class(TJvColorComponent)
  private
    FArrowPosition: TJvArrowPosition;
    FColorOrientation: TJvColorOrientation;
    FOrientation: TTrackBarOrientation;
    FBarWidth: Integer;
    FFullColorDrawing: Boolean;
    FArrowWidth: Integer;
    FArrowColor: TColor;
    FValueYAuto: Boolean;
    FValueXAuto: Boolean;
    FValueY: Byte;
    FValueX: Byte;
    FLink: TComponent;
    procedure SetArrowPosition(const Value: TJvArrowPosition);
    procedure SetColorOrientation(const Value: TJvColorOrientation);
    procedure SetOrientation(const Value: TTrackBarOrientation);
    procedure SetBarWidth(const Value: Integer);
    procedure SetArrowWidth(const Value: Integer);
    procedure SetArrowColor(const Value: TColor);
    function IsValueXStored: Boolean;
    function IsValueYStored: Boolean;
    procedure SetValueX(const Value: Byte);
    procedure SetValueXAuto(const Value: Boolean);
    procedure SetValueY(const Value: Byte);
    procedure SetValueYAuto(const Value: Boolean);
    procedure UpdateDefaultValueX;
    procedure UpdateDefaultValueY;
    procedure SetFullColorDrawing(const Value: Boolean);
  protected
    procedure MouseColor(Shift: TShiftState; X, Y: Integer); override;
    procedure SetFullColor(const Value: TJvFullColor); override;
    procedure CalcSize; override;
    procedure DrawBuffer; override;
    procedure ColorSpaceChange; override;
    procedure AxisConfigChange; override;
    procedure KeyMove(KeyCode: TKeyCode; MoveCount: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;

    function Linked: Boolean;
    function LinkerName: TComponentName;
    procedure SetLink(AComponent: TComponent);
    procedure FreeLink;
  published
    property ArrowColor: TColor read FArrowColor write SetArrowColor default clBlack;
    property ArrowWidth: Integer read FArrowWidth write SetArrowWidth default 9;
    property ArrowPosition: TJvArrowPosition read FArrowPosition write SetArrowPosition default apTop;
    property ColorOrientation: TJvColorOrientation read FColorOrientation write SetColorOrientation default coLeftToRight;
    property Orientation: TTrackBarOrientation read FOrientation write SetOrientation default trHorizontal;
    property BarWidth: Integer read FBarWidth write SetBarWidth default 10;
    property ValueX: Byte read FValueX write SetValueX stored IsValueXStored;
    property ValueXAuto: Boolean read FValueXAuto write SetValueXAuto stored False;
    property ValueY: Byte read FValueY write SetValueY stored IsValueYStored;
    property ValueYAuto: Boolean read FValueYAuto write SetValueYAuto stored False;
    property FullColorDrawing: Boolean read FFullColorDrawing write SetFullColorDrawing default True;
  end;

  TJvShapePosition = (spLeft, spRight, spTop, spBottom);

  TJvColorLabel = class(TGraphicControl)
  private
    FBrush: TBrush;
    FFont: TFont;
    FPen: TPen;
    FCaption: TCaption;
    FShapeType: TShapeType;
    FShapeWidth: Integer;
    FShapeHeight: Integer;
    FShapePosition: TJvShapePosition;
    FSpacing: Integer;
    FRoundShapeWidth: Integer;
    FRoundShapeHeight: Integer;
    FLabelColor: TJvFullColor;
    procedure SetCaption(const Value: TCaption);
    procedure SetShapeType(const Value: TShapeType);
    procedure SetShapeHeight(const Value: Integer);
    procedure SetShapePosition(const Value: TJvShapePosition);
    procedure SetShapeWidth(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetRoundShapeHeight(const Value: Integer);
    procedure SetRoundShapeWidth(const Value: Integer);
    procedure SetLabelColor(const Value: TJvFullColor);
    procedure SetBrush(const Value: TBrush);
    procedure SetFont(const Value: TFont);
    procedure SetPen(const Value: TPen);
  protected
    procedure Paint; override;
    procedure CalcSize;
    procedure SetAutoSize(Value: Boolean); override;
    procedure Resize; override;
    procedure GraphicChange(Sender: TObject);
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LabelColor: TJvFullColor read FLabelColor write SetLabelColor default fclDEFWindowText;
    property AutoSize;
    property Font: TFont read FFont write SetFont;
    property Pen: TPen read FPen write SetPen;
    property Brush: TBrush read FBrush write SetBrush;
    property Shape: TShapeType read FShapeType write SetShapeType default stRectangle;
    property Caption: TCaption read FCaption write SetCaption;
    property ShapeWidth: Integer read FShapeWidth write SetShapeWidth default 16;
    property ShapeHeight: Integer read FShapeHeight write SetShapeHeight default 16;
    property ShapePosition: TJvShapePosition read FShapePosition write SetShapePosition default spLeft;
    property Spacing: Integer read FSpacing write SetSpacing default 5;
    property RoundShapeWidth: Integer read FRoundShapeWidth write SetRoundShapeWidth default 4;
    property RoundShapeHeight: Integer read FRoundShapeHeight write SetRoundShapeHeight default 4;
  end;

  TJvFullColors = array [0..MaxListSize - 1] of TJvFullColor;
  PJvFullColors = ^TJvFullColors;

  TJvFullColorListEvent = procedure(Sender: TObject; Item: TJvFullColor; Position: Integer) of object;

  EJvFullColorListError = class(EJVCLException);

  TJvFullColorList = class(TPersistent)
  private
    FCapacity: Integer;
    FCount: Integer;
    FList: PJvFullColors;
    FOnChange: TJvFullColorListEvent;
    FUpdateCount: Integer;
    procedure SetCapacity(const Value: Integer);
    procedure SetCount(const Value: Integer);
  protected
    procedure Grow;
    function GetItem(Index: Integer): TJvFullColor;
    procedure SetItem(Index: Integer; const Value: TJvFullColor);
    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteItems(Writer: TWriter);
    procedure ReadItems(Reader: TReader);
    procedure Change(AColor: TJvFullColor; AIndex: Integer);
  public
    destructor Destroy; override;

    function Add(AColor: TJvFullColor): Integer;
    procedure Clear;
    function Remove(AColor: TJvFullColor): Integer;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Insert(Index: Integer; AColor: TJvFullColor);
    function IndexOf(AColor: TJvFullColor): Integer;
    procedure DeleteRedundant;
    procedure BeginUpdate;
    procedure EndUpdate;

    property Items[Index: Integer]: TJvFullColor read GetItem write SetItem; default;
    property List: PJvFullColors read FList;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property UpdateCount: Integer read FUpdateCount;
    property OnChange: TJvFullColorListEvent read FOnChange write FOnChange;
  end;

  EJvColorError = class(EJVCLException);

  TJvColorSpaceFormat = (cfName, cfShortName, cfBoth);

  TJvColorSpaceCombo = class(TCustomComboBox)
  private
    FAllowVariable: Boolean;
    FItemFormat: TJvColorSpaceFormat;
    function GetColorSpace: TJvColorSpace;
    procedure SetAllowVariable(const Value: Boolean);
    procedure SetColorSpace(const Value: TJvColorSpace);
    procedure SetColorSpaceID(const Value: TJvColorSpaceID);
    function GetColorSpaceID: TJvColorSpaceID;
    procedure SetItemFormat(const Value: TJvColorSpaceFormat);
  protected
    procedure MakeList; virtual;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    property SelectedSpace: TJvColorSpace read GetColorSpace write SetColorSpace;
  published
    property AllowVariable: Boolean read FAllowVariable write SetAllowVariable default True;
    // (rom) set default value
    property ColorSpaceID: TJvColorSpaceID read GetColorSpaceID write SetColorSpaceID default csRGB;
    property ItemFormat: TJvColorSpaceFormat read FItemFormat write SetItemFormat default cfBoth;
    property AutoDropDown;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ParentBiDiMode;
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
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;

  TJvColorAxisConfigFormat = (afShort, afIndent, afComplete);

  TJvColorAxisConfigCombo = class(TCustomComboBox)
  private
    FItemFormat: TJvColorAxisConfigFormat;
    FColorID: TJvColorSpaceID;
    procedure SetItemFormat(const Value: TJvColorAxisConfigFormat);
    procedure SetSelected(const Value: TJvColorAxisConfig);
    procedure SetColorID(const Value: TJvColorSpaceID);
    function GetSelected: TJvColorAxisConfig;
  protected
    procedure MakeList; virtual;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ItemFormat: TJvColorAxisConfigFormat read FItemFormat write SetItemFormat default afComplete;
    property Selected: TJvColorAxisConfig read GetSelected write SetSelected;
    property ColorID: TJvColorSpaceID read FColorID write SetColorID default csRGB;
    property AutoDropDown;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ParentBiDiMode;
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
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;

function GetIndexAxis(AxisConfig: TJvColorAxisConfig; AxisID: TJvAxisIndex): TJvAxisIndex;
function GetIndexAxisX(AxisConfig: TJvColorAxisConfig): TJvAxisIndex;
function GetIndexAxisY(AxisConfig: TJvColorAxisConfig): TJvAxisIndex;
function GetIndexAxisZ(AxisConfig: TJvColorAxisConfig): TJvAxisIndex;
function ColorSpaceToString(AColorSpace: TJvColorSpace;
  ItemFormat: TJvColorSpaceFormat): string;
function AxisConfigToString(AxisConfig: TJvColorAxisConfig;
  ItemFormat: TJvColorAxisConfigFormat; AColorSpace: TJvColorSpace): string;

procedure Register;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_RTLCONSTS}
  RTLConsts,
  {$ENDIF HAS_UNIT_RTLCONSTS}
  Math, TypInfo, GraphUtil;

resourcestring
  RsEDuplicateTrackBar = 'TrackBar already used by component "%s"';
  //RsEDuplicateTrackBar = 'TrackBar déjà utilisée par le composant "%s"';

const
  MaxPixelCount = 32767;
type
  PFullColorArray = ^TFullColorArray;
  TFullColorArray = array [0..MaxPixelCount] of TJvFullColor;

type
  TJvColorAxisConfigs = array [TJvAxisIndex] of TJvAxisIndex;

const
  TabAxisConfigs: array [TJvColorAxisConfig] of TJvColorAxisConfigs =
   ((axIndex0, axIndex1, axIndex2),
    (axIndex0, axIndex2, axIndex1),
    (axIndex1, axIndex0, axIndex2),
    (axIndex2, axIndex0, axIndex1),
    (axIndex1, axIndex2, axIndex0),
    (axIndex2, axIndex1, axIndex0));

function ColorSpaceToString(AColorSpace: TJvColorSpace; ItemFormat: TJvColorSpaceFormat): string;
begin
  case ItemFormat of
    cfName:
      Result := AColorSpace.Name;
    cfShortName:
      Result := AColorSpace.ShortName;
  else
    Result := Format('%s (%s)', [AColorSpace.Name, AColorSpace.ShortName]);
  end;
end;

function AxisConfigToString(AxisConfig: TJvColorAxisConfig; ItemFormat: TJvColorAxisConfigFormat;
  AColorSpace: TJvColorSpace): string;
var
  Str: string;
  AxisConfigs: TJvColorAxisConfigs;
begin
  Str := GetEnumName(TypeInfo(TJvColorAxisConfig), Integer(AxisConfig));
  case ItemFormat of
    afShort:
      Result := Copy(Str, 3, Length(Str) - 2);
    afIndent:
      Result := Str;
  else
    AxisConfigs := TabAxisConfigs[AxisConfig];
    Result := Format('[%s] = %s ; [%s] = %s ; [%s] = %s',
      [Str[3], AColorSpace.AxisName[axIndex0], Str[4],
       AColorSpace.AxisName[axIndex1], Str[5], AColorSpace.AxisName[axIndex2]]);
  end;
end;

function GetIndexAxis(AxisConfig: TJvColorAxisConfig; AxisID: TJvAxisIndex): TJvAxisIndex;
begin
  Result := TabAxisConfigs[AxisConfig][AxisID];
end;

function GetIndexAxisX(AxisConfig: TJvColorAxisConfig): TJvAxisIndex;
begin
  Result := TabAxisConfigs[AxisConfig][axIndex0];
end;

function GetIndexAxisY(AxisConfig: TJvColorAxisConfig): TJvAxisIndex;
begin
  Result := TabAxisConfigs[AxisConfig][axIndex1];
end;

function GetIndexAxisZ(AxisConfig: TJvColorAxisConfig): TJvAxisIndex;
begin
  Result := TabAxisConfigs[AxisConfig][axIndex2];
end;

//=== { TJvColorComponent } ==================================================

constructor TJvColorComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBuffer := TBitmap.Create;
  FBuffer.PixelFormat := pf32Bit;
  FAutoMouse := True;
  FAxisConfig := acXYZ;

  FFullColor := fclRGBWhite;

  TabStop := True;

  ControlStyle := [csSetCaption, csOpaque];
end;

destructor TJvColorComponent.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

procedure TJvColorComponent.CalcSize;
begin
  WantDrawBuffer := True;
end;

procedure TJvColorComponent.DrawBuffer;
begin
  Invalidate;
end;

procedure TJvColorComponent.Paint;
begin
  if WantDrawBuffer then
    DrawBuffer;
  WantDrawBuffer := False;
  inherited Paint;
end;

procedure TJvColorComponent.DrawFocus;
begin
  if Focused and not (csDesigning in ComponentState) then
    with Canvas do
    begin
      Pen.Color := Color;
      Brush.Color := Color;
      DrawFocusRect(ClientRect);
    end;
end;

procedure TJvColorComponent.DrawFrame(X, Y: Integer);
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0, 0, Width, Y));
  Canvas.FillRect(Rect(0, Y+FBuffer.Height, Width, Height));
  Canvas.FillRect(Rect(0, Y, X, Y+FBuffer.Height));
  Canvas.FillRect(Rect(X+FBuffer.Width, Y, Width, Y+FBuffer.Height));
end;

procedure TJvColorComponent.SetFullColor(const Value: TJvFullColor);
var
  CurrentColorID: TJvColorSpaceID;
begin
  CurrentColorID := ColorSpaceManager.GetColorSpaceID(FFullColor);
  FFullColor := Value;
  if CurrentColorID <> ColorSpaceManager.GetColorSpaceID(FFullColor) then
    ColorSpaceChange;

  if Assigned(FOnColorChange) then
    FOnColorChange(Self);
end;

procedure TJvColorComponent.MouseColor(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseColor) then
    FOnMouseColor(Self, X, Y);
end;

procedure TJvColorComponent.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SetFocus;
  try
    if AutoMouse and (Shift * [ssLeft, ssMiddle, ssRight] <> []) then
      MouseColor(Shift, X, Y);
    inherited MouseDown(Button, Shift, X, Y);
  finally
    SetCapture(Handle);
  end;
end;

procedure TJvColorComponent.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if AutoMouse and (Shift * [ssLeft, ssMiddle, ssRight] <> []) then
    MouseColor(Shift, X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvColorComponent.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  try
    inherited MouseUp(Button, Shift, X, Y);
  finally
    ReleaseCapture;
  end;
end;

procedure TJvColorComponent.SetAxisConfig(const Value: TJvColorAxisConfig);
begin
  if FAxisConfig <> Value then
  begin
    FAxisConfig := Value;
    AxisConfigChange;
  end;
end;

procedure TJvColorComponent.ColorSpaceChange;
begin
  CalcSize;
  if Assigned(FOnColorSpaceChange) then
    FOnColorSpaceChange(Self);
end;

function TJvColorComponent.GetColorSpace: TJvColorSpace;
begin
  with ColorSpaceManager do
    Result := ColorSpace[GetColorSpaceID(FullColor)];
end;

procedure TJvColorComponent.AxisConfigChange;
begin
  CalcSize;
  if Assigned(FOnAxisConfigChange) then
    FOnAxisConfigChange(Self);
end;

procedure TJvColorComponent.SetWantDrawBuffer(Value: Boolean);
begin
  FWantDrawBuffer := Value;
  if Value then
    Invalidate;
end;

procedure TJvColorComponent.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TJvColorComponent.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;

procedure TJvColorComponent.DoExit;
begin
  inherited DoExit;
  Invalidate;
end;

procedure TJvColorComponent.KeyMove(KeyCode: TKeyCode; MoveCount: Integer);
begin
  Update;
end;

procedure TJvColorComponent.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_LEFT:
      KeyMove(kcLeft, 1);
    VK_RIGHT:
      KeyMove(kcRight, 1);
    VK_UP:
      KeyMove(kcUp, 1);
    VK_DOWN:
      KeyMove(kcDown, 1);
  end;
end;

procedure TJvColorComponent.CMColorChanged(var Msg: TMessage);
begin
  inherited;
  WantDrawBuffer := True;
end;

procedure TJvColorComponent.CMSysColorChange(var Msg: TMessage);
begin
  inherited;
  WantDrawBuffer := True;
end;

//=== { TColor2D } ===========================================================

constructor TJvColorComponent2D.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValueZ := 0;
  FValueZAuto := True;
  ColorSpaceChange;
end;

procedure TJvColorComponent2D.AxisConfigChange;
begin
  UpdateDefaultValueZ;
  inherited AxisConfigChange;
end;

procedure TJvColorComponent2D.ColorSpaceChange;
begin
  UpdateDefaultValueZ;
  inherited ColorSpaceChange;
end;

procedure TJvColorComponent2D.TrackBarAxisConfigChange(Sender: TObject);
begin
  if not FAxisConfigChanging then
  begin
    FAxisConfigChanging := True;
    AxisConfig := (Sender as TJvFullColorTrackBar).AxisConfig;
    FAxisConfigChanging := False;
  end;
end;

procedure TJvColorComponent2D.TrackBarColorChange(Sender: TObject);
begin
  if FColorChanging then
    Exit;

  FColorChanging := True;
  FullColor := (Sender as TJvFullColorTrackBar).FullColor;
  FColorChanging := False;

  if Assigned(FOnColorChange) then
    FOnColorChange(Self);
end;

function TJvColorComponent2D.IsValueZStored: Boolean;
begin
  Result := not ValueZAuto;
end;

procedure TJvColorComponent2D.SetValueZ(const Value: Byte);
begin
  FValueZAuto := False;
  FValueZ := Value;
  WantDrawBuffer := True;
end;

procedure TJvColorComponent2D.SetValueZAuto(const Value: Boolean);
begin
  FValueZAuto := Value;
  if Value then
    UpdateDefaultValueZ;
  WantDrawBuffer := True;
end;

procedure TJvColorComponent2D.UpdateDefaultValueZ;
begin
  FValueZ := ColorSpace.AxisDefault[GetIndexAxisZ(AxisConfig)];
end;

//=== { TJvColorPanel } ======================================================

constructor TJvColorPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCrossSize := 5;
  FCrossCenter := 1;
  FReverseAxisX := False;
  FReverseAxisY := False;
  FPen := TPen.Create;
  FPen.OnChange := PenChange;
  FColorChanging := False;
end;

destructor TJvColorPanel.Destroy;
begin
  ColorTrackBar := nil;
  FPen.Free;
  inherited Destroy;
end;

procedure TJvColorPanel.CalcSize;
var
  LWidth, LHeight: Integer;
  IndexX, IndexY: TJvAxisIndex;
begin
  IndexX := GetIndexAxisX(AxisConfig);
  IndexY := GetIndexAxisY(AxisConfig);

  with ColorSpace do
  begin
    LWidth := AxisMax[IndexX] - AxisMin[IndexX] + 1;
    LHeight := AxisMax[IndexY] - AxisMin[IndexY] + 1;
  end;

  FBuffer.Width := LWidth;
  FBuffer.Height := LHeight;

  SetBounds(Left, Top, 2*FCrossSize + LWidth, 2*FCrossSize + LHeight);

  inherited CalcSize;
end;

procedure TJvColorPanel.DrawBuffer;
var
  AxisX, AxisY: TJvAxisIndex;
  IndexX, IndexY: Byte;
  MinX, MaxX, MinY, MaxY: Byte;
  TempColor: TJvFullColor;
  Line: PFullColorArray;
  X, Y, I: Integer;
begin
  AxisX := GetIndexAxisX(AxisConfig);
  AxisY := GetIndexAxisY(AxisConfig);

  if ColorSpace.ID = csDEF then
    with FBuffer.Canvas do
    begin
      Brush.Color := Color;
      FillRect(Rect(0, 0, Width, Height));
      Pen.Color := clBlack;
      X := 8;
      Y := 8;
      {
      for I := Low(ColorValues) to High(ColorValues) do
      begin
        Brush.Color := ColorValues[I].Value;
        Rectangle(X, Y, X+16, Y+16);
        Inc(X, 16+6);
        if X > FBuffer.Width - 8 then
        begin
          X := 8;
          Inc(Y, 16+6);
        end;
      end;
      X := 8;
      Inc(Y, 16+6);
      }
      for I := Low(SysColorValues) to High(SysColorValues) do
      begin
        Brush.Color := SysColorValues[I].Value;
        Rectangle(X, Y, X+16, Y+16);
        Inc(X, 16+6);
        if X > FBuffer.Width - 8 then
        begin
          X := 8;
          Inc(Y, 16+6);
        end;
      end;
    end
  else
    with ColorSpace do
    begin
      MinX := AxisMin[AxisX];
      MaxX := AxisMax[AxisX];
      MinY := AxisMin[AxisY];
      MaxY := AxisMax[AxisY];

      TempColor := SetAxisValue(fclRGBBlack, GetIndexAxisZ(AxisConfig), ValueZ);
      with FBuffer do
      begin
        Canvas.Brush.Color := Color;
        Canvas.FillRect(Rect(0, 0, Width, Height));
        for IndexY := MinY to MaxY do
        begin
          if ReverseAxisY then
            Line := ScanLine[MaxY - IndexY]
          else
            Line := ScanLine[IndexY - MinY];

          TempColor := SetAxisValue(TempColor, AxisY, IndexY);

          for IndexX := MinX to MaxX do
          begin
            TempColor := SetAxisValue(TempColor, AxisX, IndexX);
            // RGBToColor?
            if ReverseAxisX then
              Line[MaxX - IndexX] := ConvertToColor(TempColor)
            else
              Line[IndexX - MinX] := ConvertToColor(TempColor);
          end;
        end;
      end;
    end;

  inherited DrawBuffer;
end;

procedure TJvColorPanel.Paint;
var
  PosX, PosY: Integer;
  AxisX, AxisY: TJvAxisIndex;
begin
  inherited Paint;

  with Canvas do
  begin
    Brush.Color := Color;
    DrawFrame(CrossSize, CrossSize);
    Draw(CrossSize, CrossSize, FBuffer);
    Pen := CrossStyle;

    AxisX := GetIndexAxisX(AxisConfig);
    AxisY := GetIndexAxisY(AxisConfig);
    with ColorSpace do
    begin
      PosX := GetAxisValue(FullColor, AxisX);
      if ReverseAxisX then
        PosX := AxisMax[AxisX] - PosX
      else
        PosX := PosX - AxisMin[AxisX];
      PosX := PosX + CrossSize;

      PosY := GetAxisValue(FullColor, AxisY);
      if ReverseAxisY then
        PosY := AxisMax[AxisY] - PosY
      else
        PosY := PosY - AxisMin[AxisY];
      PosY := PosY + CrossSize;

      MoveTo(PosX - CrossSize, PosY);
      LineTo(PosX - CrossCenter, PosY);
      MoveTo(PosX + CrossCenter, PosY);
      LineTo(PosX + CrossSize, PosY);

      MoveTo(PosX, PosY - CrossSize);
      LineTo(PosX, PosY - CrossCenter);
      MoveTo(PosX, PosY + CrossCenter);
      LineTo(PosX, PosY + CrossSize);
    end;
  end;
  DrawFocus;
end;

procedure TJvColorPanel.PenChange(Sender: TObject);
begin
  Update;
end;

procedure TJvColorPanel.SetCrossCenter(Value: Integer);
begin
  if Value >= CrossSize then
    Value := CrossSize - 1;
  if FCrossCenter <> Value then
  begin
    FCrossCenter := Value;
    Invalidate;
  end;
end;

procedure TJvColorPanel.SetCrossSize(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if FCrossCenter >= Value then
    FCrossCenter := Value - 1;
  if FCrossSize <> Value then
  begin
    FCrossSize := Value;
    CalcSize;
  end;
end;

procedure TJvColorPanel.SetReverseAxisX(const Value: Boolean);
begin
  if FReverseAxisX <> Value then
  begin
    FReverseAxisX := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvColorPanel.SetReverseAxisY(const Value: Boolean);
begin
  if FReverseAxisY <> Value then
  begin
    FReverseAxisY := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvColorPanel.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
  Invalidate;
end;

procedure TJvColorPanel.SetColorTrackBar(const Value: TJvFullColorTrackBar);
begin
  if (Value <> nil) and (Value <> FColorTrackBar) and Value.Linked then
    raise EJvColorError.CreateResFmt(@RsEDuplicateTrackBar, [Value.LinkerName]);

  if Assigned(FColorTrackBar) then
  begin
    FColorTrackBar.OnColorChange := nil;
    FColorTrackBar.OnAxisConfigChange := nil;
    FColorTrackBar.RemoveFreeNotification(Self);
    FColorTrackBar.FreeLink;
  end;

  FColorTrackBar := Value;

  if Assigned(FColorTrackBar) then
  begin
    FColorTrackBar.OnColorChange := TrackBarColorChange;
    FColorTrackBar.OnAxisConfigChange := TrackBarAxisConfigChange;
    FColorTrackBar.FullColor := FullColor;
    FColorTrackBar.AxisConfig := AxisConfig;
    FColorTrackBar.FreeNotification(Self);
    FColorTrackBar.SetLink(Self);
  end;
end;

procedure TJvColorPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = ColorTrackBar) then
    ColorTrackBar := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TJvColorPanel.SetFullColor(const Value: TJvFullColor);
var
  AxisX, AxisY: TJvAxisIndex;
  OldColor: TJvFullColor;
begin
  OldColor := FullColor;
  inherited SetFullColor(Value);

  if Assigned(FColorTrackBar) and (not FColorChanging) then
  begin
    FColorChanging := True;
    FColorTrackBar.FullColor := Value;
    FColorChanging := False;
  end;

  AxisX := GetIndexAxisX(AxisConfig);
  AxisY := GetIndexAxisY(AxisConfig);

  if (GetAxisValue(OldColor, AxisX) <> GetAxisValue(FullColor, AxisX)) or
    (GetAxisValue(OldColor, AxisY) <> GetAxisValue(FullColor, AxisY)) then
    Update;
  if (ColorSpaceManager.GetColorSpaceID(OldColor) <> ColorSpaceManager.GetColorSpaceID(FullColor)) then
    CalcSize;
end;

procedure TJvColorPanel.MouseColor(Shift: TShiftState; X, Y: Integer);
var
  MinX, MaxX, MinY, MaxY: Byte;
  AxisX, AxisY: TJvAxisIndex;
  I, PosX, PosY: Integer;
begin
  if not (ssLeft in Shift) then
    Exit;

  if ColorSpace.ID = csDEF then
  begin
    Dec(X, CrossSize);
    Dec(Y, CrossSize);
    PosX := 8;
    PosY := 8;
    {
    for I := Low(ColorValues) to High(ColorValues) do
    begin
      if (X >= PosX) and (X < PosX+16) and (Y >= PosY) and (Y < PosY+16) then
        FullColor := ColorValues[I].Value;
      Inc(X, 16+6);
      if PosX > FBuffer.Width - 8 then
      begin
        PosX := 8;
        Inc(PosY, 16+6);
      end;
    end;
    PosX := 8;
    Inc(PosY, 16+6);
    }
    for I := Low(SysColorValues) to High(SysColorValues) do
    begin
      if (X >= PosX) and (X < PosX+16) and (Y >= PosY) and (Y < PosY+16) then
        FullColor := SysColorValues[I].Value;
      Inc(PosX, 16+6);
      if PosX > FBuffer.Width - 8 then
      begin
        PosX := 8;
        Inc(PosY, 16+6);
      end;
    end;
  end
  else
  begin
    AxisX := GetIndexAxisX(AxisConfig);
    AxisY := GetIndexAxisY(AxisConfig);

    with ColorSpace do
    begin
      MinX := AxisMin[AxisX];
      MaxX := AxisMax[AxisX];
      MinY := AxisMin[AxisY];
      MaxY := AxisMax[AxisY];

      PosX := EnsureRange(X - CrossSize, 0, MaxX - MinX);
      if ReverseAxisX then
        PosX := MaxX - PosX
      else
        PosX := PosX - MinX;
      PosX := PosX + MinX;

      PosY := EnsureRange(Y - CrossSize, 0, MaxY - MinY);
      if ReverseAxisY then
        PosY := MaxY - PosY
      else
        PosY := PosY - MinY;
      PosY := PosY + MinY;

      FullColor := SetAxisValue(SetAxisValue(FullColor, AxisX, Byte(PosX)), AxisY, Byte(PosY));
    end;
  end;
  inherited MouseColor(Shift, X, Y);
end;

procedure TJvColorPanel.AxisConfigChange;
begin
  if (FColorTrackBar <> nil) and not FAxisConfigChanging then
  begin
    FAxisConfigChanging := True;
    FColorTrackBar.AxisConfig := AxisConfig;
    FAxisConfigChanging := False;
  end;

  inherited AxisConfigChange;
end;

procedure TJvColorPanel.KeyMove(KeyCode: TKeyCode; MoveCount: Integer);
var
  IndexAxisX, IndexAxisY: TJvAxisIndex;
  ValueX, ValueY: Integer;
begin
  IndexAxisX := GetIndexAxisX(AxisConfig);
  IndexAxisY := GetIndexAxisY(AxisConfig);
  ValueX := GetAxisValue(FullColor, IndexAxisX);
  ValueY := GetAxisValue(FullColor, IndexAxisY);

  case KeyCode of
    kcLeft:
      begin
        if ReverseAxisX then
          MoveCount := -MoveCount;
        ValueX := ValueX - MoveCount;
      end;
    kcRight:
      begin
        if ReverseAxisX then
          MoveCount := -MoveCount;
        ValueX := ValueX + MoveCount;
      end;
    kcUp:
      begin
        if ReverseAxisY then
          MoveCount := -MoveCount;
        ValueY := ValueY - MoveCount;
      end;
    kcDown:
      begin
        if ReverseAxisY then
          MoveCount := -MoveCount;
        ValueY := ValueY + MoveCount;
      end;
  end;

  with ColorSpace do
  begin
    ValueX := EnsureRange(ValueX, AxisMin[IndexAxisX], AxisMax[IndexAxisX]);
    ValueY := EnsureRange(ValueY, AxisMin[IndexAxisY], AxisMax[IndexAxisY]);
  end;

  FullColor := SetAxisValue(SetAxisValue(FullColor, IndexAxisX, ValueX), IndexAxisY, ValueY);
end;

//=== { TJvColorCircle } =====================================================

constructor TJvColorCircle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCrossStyle := TPen.Create;
  FCrossStyle.OnChange := PenChanged;
  FInvertRadius := False;
  InvertRotation := False;
  FCrossSize := 5;
  FCrossCenter := 1;
  FLineWidth := 1;
  FRedColor := fclRGBRed;
  FGreenColor := fclRGBLime;
  FBlueColor := fclRGBBlue;
  FDraggingColor := rcCommon;
  FCrossGreenColor := clGreen;
  FCrossRedColor := clMaroon;
  FCrossBlueColor := clNavy;
  FStyles := [crShowLines, crShowRed, crShowGreen, crShowBlue];
end;

destructor TJvColorCircle.Destroy;
begin
  FCrossStyle.Free;
  inherited Destroy;
end;

procedure TJvColorCircle.CalcSize;
var
  RadiusIndex: TJvAxisIndex;
  Diameter: Integer;
begin
  RadiusIndex := GetIndexAxisX(AxisConfig);
  with ColorSpace do
    Diameter := 2 * (AxisMax[RadiusIndex] - AxisMin[RadiusIndex]) + 1;

  SetBounds(Left, Top, Diameter + 2*CrossSize, Diameter + 2*CrossSize);

  FBuffer.Width := Diameter;
  FBuffer.Height := Diameter;

  inherited CalcSize;
end;

procedure TJvColorCircle.DrawBuffer;
var
  X, Y, Radius, Angle, MaxRadius, MinRadius: Integer;
  AxisRadius, AxisAngle: TJvAxisIndex;
  MaxAngle, MinAngle, Radius1, Radius2: Integer;
  SqrYRadius1: Integer;
  AngleUnit, AngleUnitPi, YRadius1: Extended;
  Magic1, Magic2, Magic3: Byte;
  Line: PFullColorArray;
begin
  AxisRadius := GetIndexAxisX(AxisConfig);
  AxisAngle := GetIndexAxisY(AxisConfig);

  with ColorSpace do
  begin
    MaxRadius := AxisMax[AxisRadius];
    MinRadius := AxisMin[AxisRadius];
    MaxAngle := AxisMax[AxisAngle];
    MinAngle := AxisMin[AxisAngle];
  end;

  Radius1 := MaxRadius - MinRadius;
  Radius2 := (Radius1 * 2) + 1;
  AngleUnit := (MaxAngle - MinAngle) / 2.0 / Pi;
  AngleUnitPi := (MaxAngle - MinAngle) / 2.0;

  Magic1 := ValueZ;
  Magic2 := Magic1;
  Magic3 := Magic1;

  with FBuffer, ColorSpace do
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    for Y := 0 to Radius2 - 1 do
    begin
      Line := ScanLine[Y];
      SqrYRadius1 := Sqr(Y - Radius1);
      YRadius1 := Y - Radius1;
      for X := 0 to Radius2 - 1 do
      begin
        Radius := Round(Sqrt(SqrYRadius1 + Sqr(X - Radius1))) + MinRadius;

        if Radius <= MaxRadius then
        begin
          Angle := Round(ArcTan2(YRadius1, X - Radius1)*AngleUnit + AngleUnitPi) + MinAngle;
          case AxisAngle of
            axIndex0:
              if InvertRotation then
                Magic1 := MaxAngle - Angle
              else
                Magic1 := Angle + MinAngle;
            axIndex1:
              if InvertRotation then
                Magic2 := MaxAngle - Angle
              else
                Magic2 := Angle + MinAngle;
            axIndex2:
              if InvertRotation then
                Magic3 := MaxAngle - Angle
              else
                Magic3 := Angle + MinAngle;
          end;
          case AxisRadius of
            axIndex0:
              if InvertRadius then
                Magic1 := MaxRadius - Radius
              else
                Magic1 := Radius + MinRadius;
            axIndex1:
              if InvertRadius then
                Magic2 := MaxRadius - Radius
              else
                Magic2 := Radius + MinRadius;
            axIndex2:
              if InvertRadius then
                Magic3 := MaxRadius - Radius
              else
                Magic3 := Radius + MinRadius;
          end;

          Line[X] := ConvertToColor(Magic1 or (Magic2 shl 8) or (Magic3 shl 16));
        end;
      end;
    end;
  end;

  inherited DrawBuffer;
end;

procedure TJvColorCircle.Paint;
var
  Radius1: Integer;
  RadiusIndex: TJvAxisIndex;

  procedure DrawCross(AFullColor: TJvFullColor; ACrossColor: TColor);
  var
    Point: TPoint;
  begin
    Point := FullColorToPosition(AFullColor);

    with Canvas do
    begin
      Pen := CrossStyle;
      Pen.Color := ACrossColor;

      MoveTo(Point.X, Point.Y + CrossSize);
      LineTo(Point.X + CrossSize - CrossCenter, Point.Y + CrossSize);

      MoveTo(Point.X + CrossSize + CrossCenter, Point.Y + CrossSize);
      LineTo(Point.X + (2 * CrossSize), Point.Y + CrossSize);

      MoveTo(Point.X + CrossSize, Point.Y);
      LineTo(Point.X + CrossSize, Point.Y + CrossSize - CrossCenter);

      MoveTo(Point.X + CrossSize, Point.Y + CrossSize + CrossCenter);
      LineTo(Point.X + CrossSize, Point.Y + (2 * CrossSize));

      Pen.Mode := pmCopy;
      Pen.Style := psSolid;
      Pen.Width := LineWidth;
      MoveTo(Radius1 + CrossSize, Radius1 + CrossSize);
      LineTo(Point.X + CrossSize, Point.Y + CrossSize);
    end;
  end;

begin
  inherited Paint;

  RadiusIndex := GetIndexAxisX(AxisConfig);
  with ColorSpace do
    Radius1 := AxisMax[RadiusIndex] - AxisMin[RadiusIndex];

  with Canvas do
  begin
    Brush.Color := Color;
    DrawFrame(CrossSize, CrossSize);
    Draw(CrossSize, CrossSize, FBuffer);

    if crShowCommon in Styles then
      DrawCross(FullColor, CrossStyle.Color)
    else
    begin
      if crShowBlue in Styles then
        DrawCross(BlueColor, CrossBlueColor);
      if crShowRed in Styles then
        DrawCross(RedColor, CrossRedColor);
      if crShowGreen in Styles then
        DrawCross(GreenColor, CrossGreenColor);
    end;
  end;
  DrawFocus;
end;

function TJvColorCircle.FullColorToPosition(AFullColor: TJvFullColor): TPoint;
var
  ColorID: TJvColorSpaceID;
  RadiusIndex, AngleIndex: TJvAxisIndex;
  Radius, RadiusMax, RadiusMin, Angle, AngleMax, AngleMin: Integer;
  Radius1: Integer;
  FullAngle: Extended;
begin
  with ColorSpaceManager do
  begin
    ColorID := GetColorSpaceID(AFullColor);
    if ColorID <> GetColorSpaceID(AFullColor) then
      AFullColor := ConvertToID(AFullColor, ColorID);
  end;

  with ColorSpace do
  begin
    RadiusIndex := GetIndexAxisX(AxisConfig);
    Radius := GetAxisValue(AFullColor, RadiusIndex);
    RadiusMax := AxisMax[RadiusIndex];
    RadiusMin := AxisMin[RadiusIndex];

    AngleIndex := GetIndexAxisY(AxisConfig);
    Angle := GetAxisValue(AFullColor, AngleIndex);
    AngleMax := AxisMax[AngleIndex];
    AngleMin := AxisMin[AngleIndex];
  end;

  Radius1 := RadiusMax - RadiusMin;

  if InvertRadius then
    Radius := RadiusMax - Radius
  else
    Radius := Radius - RadiusMin;
  if InvertRotation then
    Angle := AngleMax - Angle
  else
    Angle := Angle - AngleMin;

  FullAngle := (2 * Pi * Angle / (AngleMax - AngleMin)) - Pi;
  Result.X := Round(Radius * Cos(FullAngle)) + Radius1;
  Result.Y := Round(Radius * Sin(FullAngle)) + Radius1;
end;

function TJvColorCircle.PositionToFullColor(APoint: TPoint): TJvFullColor;
var
  RadiusIndex, AngleIndex: TJvAxisIndex;
  Radius, RadiusMax, RadiusMin, Angle, AngleMax, AngleMin, Radius1: Integer;
begin
  with ColorSpace do
  begin
    RadiusIndex := GetIndexAxisX(AxisConfig);
    RadiusMax := AxisMax[RadiusIndex];
    RadiusMin := AxisMin[RadiusIndex];

    AngleIndex := GetIndexAxisY(AxisConfig);
    AngleMax := AxisMax[AngleIndex];
    AngleMin := AxisMin[AngleIndex];
  end;

  Radius1 := RadiusMax - RadiusMin;

  Radius := Round(Sqrt(Sqr(APoint.Y - Radius1) + Sqr(APoint.X - Radius1)));
  Angle := Round((ArcTan2(APoint.Y - Radius1, APoint.X - Radius1) + Pi) * (AngleMax - AngleMin) / 2.0 / Pi);

  if InvertRadius then
    Radius := RadiusMax - Radius
  else
    Radius := Radius + RadiusMin;
  if InvertRotation then
    Angle := AngleMax - Angle
  else
    Angle := Angle + AngleMin;

  Radius := EnsureRange(Radius, RadiusMin, RadiusMax);
  Angle := EnsureRange(Angle, AngleMin, AngleMax);

  Result := SetAxisValue(
    SetAxisValue(
      SetAxisValue(ColorSpace.ID shl 24, GetIndexAxisZ(AxisConfig), ValueZ),
      AngleIndex, Angle), RadiusIndex, Radius);
end;

procedure TJvColorCircle.MouseColor(Shift: TShiftState; X, Y: Integer);
var
  LFullColor: TJvFullColor;

  function MoveColor(var AFullColor: TJvFullColor): Boolean;
  var
    Distance: Integer;
    Point: TPoint;
  begin
    Point := FullColorToPosition(LFullColor);
    Distance := Round(Sqrt(Sqr(X - CrossSize - Point.X) + Sqr(Y - CrossSize - Point.Y)));
    if Distance < CrossSize then
    begin
      AFullColor := LFullColor;
      Result := True;
      Update;
    end
    else
      Result := False;
  end;

begin
  LFullColor := PositionToFullColor(Point(X - CrossSize, Y - CrossSize));
  if crShowCommon in Styles then
  begin
    if (ssLeft in Shift) or
      ((cr3ButtonsMouse in Styles) and (cr3ButtonsCommon in Styles)) then
      FullColor := LFullColor;
  end
  else
  if cr3ButtonsMouse in Styles then
  begin
    if (ssLeft in Shift) and (crShowRed in Styles) then
      RedColor := LFullColor;
    if (ssMiddle in Shift) and (crShowGreen in Styles) then
      GreenColor := LFullColor;
    if (ssRight in Shift) and (crShowBlue in Styles) then
      BlueColor := LFullColor;
  end
  else
  begin
    if FDraggingColor = rcGreen then
      GreenColor := LFullColor
    else
    if FDraggingColor = rcRed then
      RedColor := LFullColor
    else
    if FDraggingColor = rcBlue then
      BlueColor := LFullColor
    else
    if FDraggingColor = rcCommon then
    begin
      if (crShowGreen in Styles) and MoveColor(FGreenColor) then
      begin
        FDraggingColor := rcGreen;
        if Assigned(FOnGreenColorChange) then
          FOnGreenColorChange(Self);
      end
      else
      if (crShowRed in Styles) and MoveColor(FRedColor) then
      begin
        FDraggingColor := rcRed;
        if Assigned(FOnRedColorChange) then
          FOnRedColorChange(Self);
      end
      else
      if (crShowBlue in Styles) and MoveColor(FBlueColor) then
      begin
        FDraggingColor := rcBlue;
        if Assigned(FOnBlueColorChange) then
          FOnBlueColorChange(Self);
      end;
    end;
  end;
end;

procedure TJvColorCircle.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FDraggingColor := rcCommon;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvColorCircle.PenChanged(Sender: TObject);
begin
  WantDrawBuffer := True;
end;

procedure TJvColorCircle.ConvertToID(NewFullColor: TJvFullColor);
var
  ColorID: TJvColorSpaceID;
  Change: Boolean;
begin
  with ColorSpaceManager do
  begin
    ColorID := GetColorSpaceID(NewFullColor);
    Change := ColorID <> GetColorSpaceID(FullColor);

    FFullColor := ConvertToID(FullColor, ColorID);
    FRedColor := ConvertToID(RedColor, ColorID);
    FGreenColor := ConvertToID(GreenColor, ColorID);
    FBlueColor := ConvertToID(BlueColor, ColorID);

    if Change then
      ColorSpaceChange;
  end;
end;

procedure TJvColorCircle.SetFullColor(const Value: TJvFullColor);
var
  AxisX, AxisY: TJvAxisIndex;
  OldColor: TJvFullColor;
begin
  ConvertToID(Value);

  OldColor := FullColor;
  inherited SetFullColor(Value);

  if Assigned(FCommonColorTrackBar) and not FColorChanging then
  begin
    FColorChanging := True;
    FCommonColorTrackBar.FullColor := Value;
    FColorChanging := False;
  end;

  AxisX := GetIndexAxisX(AxisConfig);
  AxisY := GetIndexAxisY(AxisConfig);

  if (GetAxisValue(OldColor, AxisX) <> GetAxisValue(FullColor, AxisX)) or
    (GetAxisValue(OldColor, AxisY) <> GetAxisValue(FullColor, AxisY)) then
    Update;
  if (ColorSpaceManager.GetColorSpaceID(OldColor) <> ColorSpaceManager.GetColorSpaceID(FullColor)) then
    CalcSize;
end;

procedure TJvColorCircle.SetBlueColor(const Value: TJvFullColor);
var
  AxisX, AxisY: TJvAxisIndex;
  OldColor: TJvFullColor;
begin
  ConvertToID(Value);

  OldColor := BlueColor;
  FBlueColor := Value;

  if Assigned(FBlueColorTrackBar) and not FColorChanging then
  begin
    FColorChanging := True;
    FBlueColorTrackBar.FullColor := Value;
    FColorChanging := False;
  end;

  AxisX := GetIndexAxisX(AxisConfig);
  AxisY := GetIndexAxisY(AxisConfig);

  if (GetAxisValue(OldColor, AxisX) <> GetAxisValue(BlueColor, AxisX)) or
    (GetAxisValue(OldColor, AxisY) <> GetAxisValue(BlueColor, AxisY)) then
    Update;

  if Assigned(FOnBlueColorChange) then
    FOnBlueColorChange(Self);
end;

procedure TJvColorCircle.SetGreenColor(const Value: TJvFullColor);
var
  AxisX, AxisY: TJvAxisIndex;
  OldColor: TJvFullColor;
begin
  ConvertToID(Value);

  OldColor := GreenColor;
  FGreenColor := Value;

  if Assigned(FGreenColorTrackBar) and not FColorChanging then
  begin
    FColorChanging := True;
    FGreenColorTrackBar.FullColor := Value;
    FColorChanging := False;
  end;

  AxisX := GetIndexAxisX(AxisConfig);
  AxisY := GetIndexAxisY(AxisConfig);

  if (GetAxisValue(OldColor, AxisX) <> GetAxisValue(GreenColor, AxisX)) or
    (GetAxisValue(OldColor, AxisY) <> GetAxisValue(GreenColor, AxisY)) then
    Update;

  if Assigned(FOnGreenColorChange) then
    FOnGreenColorChange(Self);
end;

procedure TJvColorCircle.SetRedColor(const Value: TJvFullColor);
var
  AxisX, AxisY: TJvAxisIndex;
  OldColor: TJvFullColor;
begin
  ConvertToID(Value);

  OldColor := RedColor;
  FRedColor := Value;

  if Assigned(FRedColorTrackBar) and not FColorChanging then
  begin
    FColorChanging := True;
    FRedColorTrackBar.FullColor := Value;
    FColorChanging := False;
  end;

  AxisX := GetIndexAxisX(AxisConfig);
  AxisY := GetIndexAxisY(AxisConfig);

  if (GetAxisValue(OldColor, AxisX) <> GetAxisValue(RedColor, AxisX)) or
    (GetAxisValue(OldColor, AxisY) <> GetAxisValue(RedColor, AxisY)) then
    Update;

  if Assigned(FOnRedColorChange) then
    FOnRedColorChange(Self);
end;

procedure TJvColorCircle.SetCrossCenter(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value >= CrossSize then
    Value := CrossSize - 1;
  if FCrossCenter <> Value then
  begin
    FCrossCenter := Value;
    Invalidate;
  end;
end;

procedure TJvColorCircle.SetCrossSize(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FCrossSize <> Value then
  begin
    FCrossSize := Value;
    CalcSize;
  end;
end;

procedure TJvColorCircle.SetCrossStyle(const Value: TPen);
begin
  FCrossStyle.Assign(Value);
  Invalidate;
end;

procedure TJvColorCircle.SetInvertRadius(const Value: Boolean);
begin
  if FInvertRadius <> Value then
  begin
    FInvertRadius := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvColorCircle.SetInvertRotation(const Value: Boolean);
begin
  if FInvertRotation <> Value then
  begin
    FInvertRotation := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvColorCircle.SetLineWidth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FLineWidth <> Value then
  begin
    FLineWidth := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvColorCircle.SetStyles(const Value: TJvColorCircleStyles);
begin
  if FStyles <> Value then
  begin
    FStyles := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvColorCircle.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
    if AComponent = RedTrackBar then
      RedTrackBar := nil
    else
    if AComponent = GreenTrackBar then
      GreenTrackBar := nil
    else
    if AComponent = BlueTrackBar then
      BlueTrackBar := nil
    else
    if AComponent = CommonTrackBar then
      CommonTrackBar := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TJvColorCircle.SetBlueColorTrackBar(const Value: TJvFullColorTrackBar);
begin
  if (Value <> nil) and (Value <> FBlueColorTrackBar) and Value.Linked then
    raise EJvColorError.CreateResFmt(@RsEDuplicateTrackBar, [Value.LinkerName]);

  if Assigned(FBlueColorTrackBar) then
  begin
    FBlueColorTrackBar.OnColorChange := nil;
    FBlueColorTrackBar.OnAxisConfigChange := nil;
    FBlueColorTrackBar.RemoveFreeNotification(Self);
    FBlueColorTrackBar.FreeLink;
  end;

  FBlueColorTrackBar := Value;

  if Assigned(FBlueColorTrackBar) then
  begin
    FBlueColorTrackBar.OnColorChange := TrackBarColorChange;
    FBlueColorTrackBar.OnAxisConfigChange := TrackBarAxisConfigChange;
    FBlueColorTrackBar.FullColor := BlueColor;
    FBlueColorTrackBar.AxisConfig := AxisConfig;
    FBlueColorTrackBar.FreeNotification(Self);
    FBlueColorTrackBar.SetLink(Self);
  end;
end;

procedure TJvColorCircle.SetGreenColorTrackBar(const Value: TJvFullColorTrackBar);
begin
  if (Value <> nil) and (Value <> FGreenColorTrackBar) and Value.Linked then
    raise EJvColorError.CreateResFmt(@RsEDuplicateTrackBar, [Value.LinkerName]);

  if Assigned(FGreenColorTrackBar) then
  begin
    FGreenColorTrackBar.OnColorChange := nil;
    FGreenColorTrackBar.OnAxisConfigChange := nil;
    FGreenColorTrackBar.RemoveFreeNotification(Self);
    FGreenColorTrackBar.FreeLink;
  end;

  FGreenColorTrackBar := Value;

  if Assigned(FGreenColorTrackBar) then
  begin
    FGreenColorTrackBar.OnColorChange := TrackBarColorChange;
    FGreenColorTrackBar.OnAxisConfigChange := TrackBarAxisConfigChange;
    FGreenColorTrackBar.FullColor := GreenColor;
    FGreenColorTrackBar.AxisConfig := AxisConfig;
    FGreenColorTrackBar.FreeNotification(Self);
    FGreenColorTrackBar.SetLink(Self);
  end;
end;

procedure TJvColorCircle.SetRedColorTrackBar(const Value: TJvFullColorTrackBar);
begin
  if (Value <> nil) and (Value <> FRedColorTrackBar) and Value.Linked then
    raise EJvColorError.CreateResFmt(@RsEDuplicateTrackBar, [Value.LinkerName]);

  if Assigned(FRedColorTrackBar) then
  begin
    FRedColorTrackBar.OnColorChange := nil;
    FRedColorTrackBar.OnAxisConfigChange := nil;
    FRedColorTrackBar.RemoveFreeNotification(Self);
    FRedColorTrackBar.FreeLink;
  end;

  FRedColorTrackBar := Value;

  if Assigned(FRedColorTrackBar) then
  begin
    FRedColorTrackBar.OnColorChange := TrackBarColorChange;
    FRedColorTrackBar.OnAxisConfigChange := TrackBarAxisConfigChange;
    FRedColorTrackBar.FullColor := RedColor;
    FRedColorTrackBar.AxisConfig := AxisConfig;
    FRedColorTrackBar.FreeNotification(Self);
    FRedColorTrackBar.SetLink(Self);
  end;
end;

procedure TJvColorCircle.SetCommonColorTrackBar(const Value: TJvFullColorTrackBar);
begin
  if (Value <> nil) and (Value <> FCommonColorTrackBar) and Value.Linked then
    raise EJvColorError.CreateResFmt(@RsEDuplicateTrackBar, [Value.LinkerName]);

  if Assigned(FCommonColorTrackBar) then
  begin
    FCommonColorTrackBar.OnColorChange := nil;
    FCommonColorTrackBar.OnAxisConfigChange := nil;
    FCommonColorTrackBar.RemoveFreeNotification(Self);
    FCommonColorTrackBar.FreeLink;
  end;

  FCommonColorTrackBar := Value;

  if Assigned(FCommonColorTrackBar) then
  begin
    FCommonColorTrackBar.OnColorChange := TrackBarColorChange;
    FCommonColorTrackBar.OnAxisConfigChange := TrackBarAxisConfigChange;
    FCommonColorTrackBar.FullColor := FullColor;
    FCommonColorTrackBar.AxisConfig := AxisConfig;
    FCommonColorTrackBar.FreeNotification(Self);
    FCommonColorTrackBar.SetLink(Self);
  end;
end;

procedure TJvColorCircle.SetCrossBlueColor(const Value: TColor);
begin
  if FCrossBlueColor <> Value then
  begin
    FCrossBlueColor := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvColorCircle.SetCrossGreenColor(const Value: TColor);
begin
  if FCrossGreenColor <> Value then
  begin
    FCrossGreenColor := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvColorCircle.SetCrossRedColor(const Value: TColor);
begin
  if FCrossRedColor <> Value then
  begin
    FCrossRedColor := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvColorCircle.AxisConfigChange;
begin
  if FAxisConfigChanging then
    Exit;

  if (FCommonColorTrackBar <> nil) and
    (FCommonColorTrackBar.AxisConfig <> AxisConfig) then
  begin
    FAxisConfigChanging := True;
    FCommonColorTrackBar.AxisConfig := AxisConfig;
    FAxisConfigChanging := False;
  end;

  if (FRedColorTrackBar <> nil) and
    (FRedColorTrackBar.AxisConfig <> AxisConfig) then
  begin
    FAxisConfigChanging := True;
    FRedColorTrackBar.AxisConfig := AxisConfig;
    FAxisConfigChanging := False;
  end;

  if (FGreenColorTrackBar <> nil) and
    (FGreenColorTrackBar.AxisConfig <> AxisConfig) then
  begin
    FAxisConfigChanging := True;
    FGreenColorTrackBar.AxisConfig := AxisConfig;
    FAxisConfigChanging := False;
  end;

  if (FBlueColorTrackBar <> nil) and
    (FBlueColorTrackBar.AxisConfig <> AxisConfig) then
  begin
    FAxisConfigChanging := True;
    FBlueColorTrackBar.AxisConfig := AxisConfig;
    FAxisConfigChanging := False;
  end;

  inherited AxisConfigChange;
end;

procedure TJvColorCircle.TrackBarColorChange(Sender: TObject);
begin
  if FColorChanging then
    Exit;

  FColorChanging := True;

  if Sender = RedTrackBar then
    RedColor := (Sender as TJvFullColorTrackBar).FullColor
  else
  if Sender = GreenTrackBar then
    GreenColor := (Sender as TJvFullColorTrackBar).FullColor
  else
  if Sender = BlueTrackBar then
    BlueColor := (Sender as TJvFullColorTrackBar).FullColor
  else
  if Sender = CommonTrackBar then
  begin
    FullColor := (Sender as TJvFullColorTrackBar).FullColor;
    if Assigned(FOnColorChange) then
      FOnColorChange(Self);
  end;

  FColorChanging := False;
end;

procedure TJvColorCircle.ColorSpaceChange;
begin
  if CommonTrackBar <> nil then
    CommonTrackBar.FullColor := FullColor;
  if RedTrackBar <> nil then
    RedTrackBar.FullColor := RedColor;
  if GreenTrackBar <> nil then
    GreenTrackBar.FullColor := GreenColor;
  if BlueTrackBar <> nil then
    BlueTrackBar.FullColor := BlueColor;

  inherited ColorSpaceChange;
end;

//=== { TJvFullColorTrackBar } ===============================================

constructor TJvFullColorTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOrientation := trHorizontal;
  FArrowPosition := apTop;
  FColorOrientation := coLeftToRight;

  FArrowWidth := 9;
  FArrowColor := clBlack;
  FFullColorDrawing := True;

  FValueXAuto := True;
  FValueYAuto := True;

  BarWidth := 10;

  FLink := nil;

  ColorSpaceChange;
end;

procedure TJvFullColorTrackBar.AxisConfigChange;
begin
  UpdateDefaultValueX;
  UpdateDefaultValueY;
  inherited AxisConfigChange;
end;

procedure TJvFullColorTrackBar.CalcSize;
var
  ColorAmp: Integer;
  AxisZ: TJvAxisIndex;
begin
  AxisZ := GetIndexAxisZ(AxisConfig);
  with ColorSpace do
    ColorAmp := AxisMax[AxisZ] - AxisMin[AxisZ] + 1;

  case Orientation of
    trHorizontal:
      begin
        Width := ColorAmp + (2 * ArrowWidth);
        Height := BarWidth + ArrowWidth + 1;
        FBuffer.Width := ColorAmp;
        FBuffer.Height := BarWidth;
      end;
    trVertical:
      begin
        Width := BarWidth + ArrowWidth + 1;
        Height := ColorAmp + (2 * ArrowWidth);
        FBuffer.Width := BarWidth;
        FBuffer.Height := ColorAmp;
      end;
  end;
  inherited CalcSize;
end;

procedure TJvFullColorTrackBar.ColorSpaceChange;
begin
  UpdateDefaultValueX;
  UpdateDefaultValueY;
  inherited ColorSpaceChange;
end;

procedure TJvFullColorTrackBar.DrawBuffer;
var
  AxisX, AxisY, AxisZ: TJvAxisIndex;
  MinZ, MaxZ, PosZ, IndexZ: Byte;
  TempColor: TJvFullColor;
begin
  if FCreating then
    Exit;

  AxisX := GetIndexAxisX(AxisConfig);
  AxisY := GetIndexAxisY(AxisConfig);
  AxisZ := GetIndexAxisZ(AxisConfig);

  with ColorSpace do
  begin
    MinZ := AxisMin[AxisZ];
    MaxZ := AxisMax[AxisZ];

    if FullColorDrawing then
      TempColor := FullColor
    else
      TempColor := SetAxisValue(SetAxisValue(fclRGBBlack, AxisX, ValueX), AxisY, ValueY);

    with FBuffer.Canvas do
      for IndexZ := MinZ to MaxZ do
      begin
        if ((Orientation = trHorizontal) and (ColorOrientation = coRightToLeft)) or
          ((Orientation = trVertical) and (ColorOrientation = coBottomToTop)) then
          PosZ := MaxZ - IndexZ
        else
          PosZ := IndexZ - MinZ;

        TempColor := SetAxisValue(TempColor, AxisZ, IndexZ);
        Pen.Color := ConvertToColor(TempColor);

        case Orientation of
          trHorizontal:
            begin
              MoveTo(PosZ, 0);
              LineTo(PosZ, BarWidth);
            end;
          trVertical:
            begin
              MoveTo(0, PosZ);
              LineTo(BarWidth, PosZ);
            end;
        end;
      end;
  end;

  inherited DrawBuffer;
end;

procedure TJvFullColorTrackBar.Paint;
var
  AxisZ: TJvAxisIndex;
  PosZ: Integer;
  Points: array [0..2] of TPoint;
begin
  inherited Paint;

  AxisZ := GetIndexAxisZ(AxisConfig);

  with ColorSpace do
  begin
    PosZ := GetAxisValue(FullColor, AxisZ);
    // (rom) This shift is simply not in effect. Seems to be a bug of GetAxisValue.
    // PosZ := PosZ - AxisMin[AxisZ] + ArrowWidth;
    if ((Orientation = trHorizontal) and (ColorOrientation = coRightToLeft)) then
      PosZ := FBuffer.Width - PosZ - 1
    else
    if ((Orientation = trVertical) and (ColorOrientation = coBottomToTop)) then
      PosZ := FBuffer.Height - PosZ - 1;
    Inc(PosZ, ArrowWidth);
  end;

  with Canvas do
  begin
    case Orientation of
      trHorizontal:
        begin
          Points[0].X := PosZ - ArrowWidth;
          Points[1].X := PosZ;
          Points[2].X := PosZ + ArrowWidth;
          case ArrowPosition of
            apTop:
              begin
                Points[0].Y := 0;
                Points[1].Y := ArrowWidth;
                Points[2].Y := 0;
                DrawFrame(ArrowWidth, ArrowWidth+1);
                Draw(ArrowWidth, ArrowWidth+1, FBuffer);
              end;
            apBottom:
              begin
                Points[0].Y := Height - 1;
                Points[1].Y := Height - 1 - ArrowWidth;
                Points[2].Y := Height - 1;
                DrawFrame(ArrowWidth, 0);
                Draw(ArrowWidth, 0, FBuffer);
              end;
          end;
        end;
      trVertical:
        begin
          Points[0].Y := PosZ - ArrowWidth;
          Points[1].Y := PosZ;
          Points[2].Y := PosZ + ArrowWidth;
          case ArrowPosition of
            apLeft:
              begin
                Points[0].X := 0;
                Points[1].X := ArrowWidth;
                Points[2].X := 0;
                DrawFrame(ArrowWidth+1, ArrowWidth);
                Draw(ArrowWidth+1, ArrowWidth, FBuffer);
              end;
            apRight:
              begin
                Points[0].X := Width - 1;
                Points[1].X := Width - 1 - ArrowWidth;
                Points[2].X := Width - 1;
                DrawFrame(0, ArrowWidth);
                Draw(0, ArrowWidth, FBuffer);
              end;
          end;
        end;
    end;
    Brush.Color := ArrowColor;
    Pen.Color := ArrowColor;
    Polygon(Points);
  end;
  DrawFocus;
end;

procedure TJvFullColorTrackBar.FreeLink;
begin
  FLink := nil;
end;

function TJvFullColorTrackBar.IsValueXStored: Boolean;
begin
  Result := not ValueXAuto;
end;

function TJvFullColorTrackBar.IsValueYStored: Boolean;
begin
  Result := not ValueYAuto;
end;

procedure TJvFullColorTrackBar.KeyMove(KeyCode: TKeyCode; MoveCount: Integer);
var
  IndexAxisZ: TJvAxisIndex;
  ValueZ: Integer;
begin
  IndexAxisZ := GetIndexAxisZ(AxisConfig);
  ValueZ := GetAxisValue(FullColor, IndexAxisZ);

  if (ColorOrientation = coBottomToTop) or (ColorOrientation = coRightToLeft) then
    MoveCount := -MoveCount;

  case KeyCode of
    kcLeft:
      if Orientation = trHorizontal then
        ValueZ := ValueZ - MoveCount;
    kcRight:
      if Orientation = trHorizontal then
        ValueZ := ValueZ + MoveCount;
    kcUp:
      if Orientation = trVertical then
        ValueZ := ValueZ - MoveCount;
    kcDown:
      if Orientation = trVertical then
        ValueZ := ValueZ + MoveCount;
  end;

  with ColorSpace do
    ValueZ := EnsureRange(ValueZ, AxisMin[IndexAxisZ], AxisMax[IndexAxisZ]);

  FullColor := SetAxisValue(FullColor, IndexAxisZ, ValueZ);
end;

function TJvFullColorTrackBar.Linked: Boolean;
begin
  Result := FLink <> nil;
end;

function TJvFullColorTrackBar.LinkerName: TComponentName;
begin
  Result := FLink.Name;
end;

procedure TJvFullColorTrackBar.MouseColor(Shift: TShiftState; X, Y: Integer);
var
  MinZ, MaxZ: Byte;
  AxisZ: TJvAxisIndex;
  Pos: Integer;
begin
  if not (ssLeft in Shift) then
    Exit;

  case Orientation of
    trHorizontal:
      Pos := X - ArrowWidth;
    trVertical:
      Pos := Y - ArrowWidth;
  else
    Pos := 0;
  end;

  AxisZ := GetIndexAxisZ(AxisConfig);
  with ColorSpace do
  begin
    MinZ := AxisMin[AxisZ];
    MaxZ := AxisMax[AxisZ];

    Pos := EnsureRange(Pos, 0, MaxZ - MinZ);
    if ((Orientation = trHorizontal) and (ColorOrientation = coRightToLeft)) or
      ((Orientation = trVertical) and (ColorOrientation = coBottomToTop)) then
      Pos := MaxZ - Pos
    else
      Pos := Pos - MinZ;
    Pos := Pos + MinZ;

    FullColor := SetAxisValue(FullColor, AxisZ, Byte(Pos));
  end;

  inherited MouseColor(Shift, X, Y);
end;

procedure TJvFullColorTrackBar.SetArrowColor(const Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    Update;
  end;
end;

procedure TJvFullColorTrackBar.SetArrowPosition(const Value: TJvArrowPosition);
begin
  if FArrowPosition <> Value then
  begin
    FArrowPosition := Value;
    Invalidate;
  end;
end;

procedure TJvFullColorTrackBar.SetArrowWidth(const Value: Integer);
begin
  if FArrowPosition <> Value then
  begin
    FArrowWidth := Value;
    CalcSize;
  end;
end;

procedure TJvFullColorTrackBar.SetOrientation(const Value: TTrackBarOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    CalcSize;
  end;
end;

procedure TJvFullColorTrackBar.SetBarWidth(const Value: Integer);
begin
  if FBarWidth <> Value then
  begin
    FBarWidth := Value;
    CalcSize;
  end;
end;

procedure TJvFullColorTrackBar.SetColorOrientation(const Value: TJvColorOrientation);
begin
  if FColorOrientation <> Value then
  begin
    FColorOrientation := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvFullColorTrackBar.SetFullColor(const Value: TJvFullColor);
var
  AxisZ: TJvAxisIndex;
  OldColor: TJvFullColor;
begin
  OldColor := FullColor;
  inherited SetFullColor(Value);

  if ValueXAuto then
    UpdateDefaultValueX;
  if ValueYAuto then
    UpdateDefaultValueY;

  if FullColorDrawing then
  begin
    if FullColor <> OldColor then
      WantDrawBuffer := True;
  end
  else
  begin
    AxisZ := GetIndexAxisZ(AxisConfig);
    if GetAxisValue(OldColor, AxisZ) <> GetAxisValue(FullColor, AxisZ) then
      Update;
  end;
end;

procedure TJvFullColorTrackBar.SetFullColorDrawing(const Value: Boolean);
begin
  if FFullColorDrawing <> Value then
  begin
    FFullColorDrawing := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvFullColorTrackBar.SetLink(AComponent: TComponent);
begin
  FLink := AComponent;
end;

procedure TJvFullColorTrackBar.SetValueX(const Value: Byte);
begin
  FValueX := Value;
  FValueXAuto := False;
  WantDrawBuffer := True;
end;

procedure TJvFullColorTrackBar.SetValueXAuto(const Value: Boolean);
begin
  FValueXAuto := Value;
  if Value then
    UpdateDefaultValueX;
  WantDrawBuffer := True;
end;

procedure TJvFullColorTrackBar.SetValueY(const Value: Byte);
begin
  FValueY := Value;
  FValueYAuto := False;
  WantDrawBuffer := True;
end;

procedure TJvFullColorTrackBar.SetValueYAuto(const Value: Boolean);
begin
  FValueYAuto := Value;
  if Value then
    UpdateDefaultValueY;
  WantDrawBuffer := True;
end;

procedure TJvFullColorTrackBar.UpdateDefaultValueX;
begin
  FValueX := ColorSpace.AxisDefault[GetIndexAxisX(AxisConfig)];
end;

procedure TJvFullColorTrackBar.UpdateDefaultValueY;
begin
  FValueY := ColorSpace.AxisDefault[GetIndexAxisY(AxisConfig)];
end;

//=== { TJvColorLabel } ======================================================

constructor TJvColorLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPen := TPen.Create;
  FBrush := TBrush.Create;
  FFont := TFont.Create;
  FPen.OnChange := GraphicChange;
  FBrush.OnChange := GraphicChange;
  FFont.OnChange := GraphicChange;
  FShapeType := stRectangle;
  FShapePosition := spLeft;
  FSpacing := 5;
  FRoundShapeWidth := 4;
  FRoundShapeHeight := 4;
  FShapeWidth := 16;
  FShapeHeight := 16;
  FLabelColor := fclDEFWindowText;
  //AutoSize:=True;
end;

destructor TJvColorLabel.Destroy;
begin
  FFont.Free;
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TJvColorLabel.CalcSize;
begin
  Canvas.Font := Font;
  if AutoSize then
  begin
    case ShapePosition of
      spLeft..spRight:
        begin
          Height := Max(ShapeHeight, Canvas.TextHeight(Caption));
          Width := ShapeWidth + Spacing + Canvas.TextWidth(FCaption);
        end;
      spTop..spBottom:
        begin
          Height := ShapeHeight + Spacing + Canvas.TextHeight(Caption);
          Width := Max(ShapeWidth, Canvas.TextWidth(Caption));
        end;
    end;
    AdjustSize;
  end;
end;

procedure TJvColorLabel.GraphicChange(Sender: TObject);
begin
  CalcSize;
end;

procedure TJvColorLabel.Paint;
var
  ShapeLeft, ShapeTop, TextLeft, TextTop: Integer;
begin
  ShapeLeft := 0;
  ShapeTop := 0;
  TextLeft := 0;
  TextTop := 0;
  with Canvas do
  begin
    Pen.Style := psClear;
    Brush.Style := bsSolid;
    Brush.Color := Parent.Brush.Color;
    Rectangle(0, 0, Width, Height);

    Font := Self.Font;
    Pen := Self.Pen;
    Brush := Self.Brush;
    Brush.Color := ColorSpaceManager.ConvertToID(LabelColor, csRGB);
    inherited Paint;
    case FShapePosition of
      spLeft:
        begin
          ShapeLeft := 0;
          ShapeTop := (Height - FShapeHeight) div 2;
          TextLeft := FShapeWidth + FSpacing;
          TextTop := (Height - TextHeight(FCaption)) div 2;
        end;
      spRight:
        begin
          ShapeLeft := TextWidth(FCaption) + FSpacing;
          ShapeTop := (Height - FShapeHeight) div 2;
          TextLeft := 0;
          TextTop := (Height - TextHeight(FCaption)) div 2;
        end;
      spTop:
        begin
          ShapeLeft := (Width - FShapeWidth) div 2;
          ShapeTop := 0;
          TextLeft := (Width - TextWidth(FCaption)) div 2;
          TextTop := FShapeHeight + FSpacing;
        end;
      spBottom:
        begin
          ShapeLeft := (Width - FShapeWidth) div 2;
          ShapeTop := TextHeight(FCaption) + FSpacing;
          TextLeft := (Width - TextWidth(FCaption)) div 2;
          TextTop := 0;
        end;
    end;
    case FShapeType of
      stRectangle..stSquare:
        Rectangle(ShapeLeft, ShapeTop, ShapeLeft + FShapeWidth, ShapeTop + FShapeHeight);
      stRoundRect..stRoundSquare:
        RoundRect(ShapeLeft, ShapeTop, ShapeLeft + FShapeWidth, ShapeTop + FShapeHeight,
          RoundShapeWidth, RoundShapeHeight);
      stEllipse..stCircle:
        Ellipse(ShapeLeft, ShapeTop, ShapeLeft + FShapeWidth, ShapeTop + FShapeHeight);
    end;
    Brush.Style := bsClear;
    TextOut(TextLeft, TextTop, FCaption);
  end;
end;

procedure TJvColorLabel.Resize;
begin
  inherited Resize;
  //CalcSize;
end;

procedure TJvColorLabel.SetAutoSize(Value: Boolean);
begin
  inherited SetAutoSize(Value);
  CalcSize;
end;

procedure TJvColorLabel.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    CalcSize;
  end;
end;

procedure TJvColorLabel.SetName(const Value: TComponentName);
var
  Equal: Boolean;
begin
  Equal := CompareText(Name, FCaption) = 0;
  inherited SetName(Value);
  if Equal then
    Caption := Name;
end;

procedure TJvColorLabel.SetRoundShapeHeight(const Value: Integer);
begin
  if (Value <> FRoundShapeHeight) and (Value < (ShapeHeight div 2)) then
  begin
    FRoundShapeHeight := Value;
    Invalidate;
  end;
end;

procedure TJvColorLabel.SetRoundShapeWidth(const Value: Integer);
begin
  if (Value <> FRoundShapeWidth) and (Value < (ShapeWidth div 2)) then
  begin
    FRoundShapeWidth := Value;
    Invalidate;
  end;
end;

procedure TJvColorLabel.SetShapeHeight(const Value: Integer);
begin
  if FShapeHeight <> Value then
  begin
    FShapeHeight := Value;
    if Shape in [stSquare, stRoundSquare, stCircle] then
      FShapeWidth := FShapeHeight;
    CalcSize;
  end;
end;

procedure TJvColorLabel.SetShapePosition(const Value: TJvShapePosition);
begin
  if FShapePosition <> Value then
  begin
    FShapePosition := Value;
    CalcSize;
  end;
end;

procedure TJvColorLabel.SetShapeType(const Value: TShapeType);
begin
  if FShapeType <> Value then
  begin
    FShapeType := Value;
    if Shape in [stSquare, stRoundSquare, stCircle] then
    begin
      FShapeWidth := Min(FShapeWidth, FShapeHeight);
      FShapeHeight := FShapeWidth;
    end;
    CalcSize;
  end;
end;

procedure TJvColorLabel.SetShapeWidth(const Value: Integer);
begin
  if FShapeWidth <> Value then
  begin
    FShapeWidth := Value;
    if Shape in [stSquare, stRoundSquare, stCircle] then
      FShapeHeight := FShapeWidth;
    CalcSize;
  end;
end;

procedure TJvColorLabel.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    CalcSize;
  end;
end;

procedure TJvColorLabel.SetLabelColor(const Value: TJvFullColor);
begin
  if FLabelColor <> Value then
  begin
    FLabelColor := Value;
    Brush.Color := ColorSpaceManager.ConvertToID(Value, csRGB);
    Invalidate;
  end;
end;

procedure TJvColorLabel.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
  Invalidate;
end;

procedure TJvColorLabel.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  CalcSize;
end;

procedure TJvColorLabel.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
  CalcSize;
end;

//=== { TJvFullColorList } ===================================================

destructor TJvFullColorList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJvFullColorList.Add(AColor: TJvFullColor): Integer;
begin
  Result := FCount;
  if Result = Capacity then
    Grow;
  FList^[Result] := AColor;
  Inc(FCount);
  Change(AColor, Result);
end;

procedure TJvFullColorList.Change(AColor: TJvFullColor; AIndex: Integer);
begin
  if (UpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self, AColor, AIndex);
end;

procedure TJvFullColorList.Clear;
begin
  Capacity := 0;
end;

procedure TJvFullColorList.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Items', ReadItems, WriteItems, Count > 0);
end;

procedure TJvFullColorList.Delete(Index: Integer);
var
  Color: TJvFullColor;
begin
  if (Index < 0) or (Index >= FCount) then
    EJvFullColorListError.CreateFmt(SListIndexError, [Index]);

  Color := FList^[Index];

  Dec(FCount);
  if Index < Count then
    Move(FList^[Index + 1], FList^[Index], (Count - Index) * SizeOf(TJvFullColor));

  Change(Color, -1);
end;

procedure TJvFullColorList.DeleteRedundant;
begin
end;

procedure TJvFullColorList.Exchange(Index1, Index2: Integer);
var
  Tmp: TJvFullColor;
begin
  if (Index1 >= Count) or (Index1 < 0) then
    raise EJvFullColorListError.CreateResFmt(@SListIndexError, [Index1]);

  if (Index2 >= Count) or (Index2 < 0) then
    raise EJvFullColorListError.CreateResFmt(@SListIndexError, [Index2]);

  Tmp := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Tmp;
end;

function TJvFullColorList.GetItem(Index: Integer): TJvFullColor;
begin
  if (Index >= Count) or (Index < 0) then
    raise EJvFullColorListError.CreateResFmt(@SListIndexError, [Index]);

  Result := FList^[Index];
end;

procedure TJvFullColorList.Grow;
begin
  if Capacity = 0 then
    Capacity := 16
  else
    Capacity := 2 * Capacity;
end;

function TJvFullColorList.IndexOf(AColor: TJvFullColor): Integer;
begin
  for Result := 0 to Count - 1 do
    if FList^[Result] = AColor then
      Exit;
  Result := -1;
end;

procedure TJvFullColorList.Insert(Index: Integer; AColor: TJvFullColor);
begin
  if (Index >= Count) or (Index < 0) then
    EJvFullColorListError.CreateFmt(sListIndexError, [Index]);

  if Count = Capacity then
    Grow;

  if Index < Count then
    Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(TJvFullColor));

  FList^[Index] := AColor;
  Inc(FCount);

  Change(AColor, Index);
end;

procedure TJvFullColorList.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TJvFullColorList.ReadItems(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do
      Add(Reader.ReadInteger);
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

function TJvFullColorList.Remove(AColor: TJvFullColor): Integer;
begin
  Result := IndexOf(AColor);
  if Result >= 0 then
    Delete(Result);
end;

procedure TJvFullColorList.SetCapacity(const Value: Integer);
begin
  ReallocMem(FList, Value * SizeOf(TJvFullColor));
  FCapacity := Value;
  if FCount > FCapacity then
  begin
    FCount := FCapacity;
    // (rom) this needs a comment to explain the special parameters
    Change(0, -2);
  end;
end;

procedure TJvFullColorList.SetCount(const Value: Integer);
begin
  FCount := Value;
  if FCount > FCapacity then
    Capacity := FCount;
end;

procedure TJvFullColorList.SetItem(Index: Integer; const Value: TJvFullColor);
begin
  if (Index >= Count) or (Index < 0) then
    EJvFullColorListError.CreateFmt(sListIndexError, [Index]);

  FList^[Index] := Value;
end;

procedure TJvFullColorList.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  // (rom) this needs a comment to explain the special parameters
  Change(0, -2);
end;

procedure TJvFullColorList.WriteItems(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do
    Writer.WriteInteger(FList^[I]);
  Writer.WriteListEnd;
end;

//=== { TJvColorSpaceCombo } =================================================

constructor TJvColorSpaceCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csDropDownList;
  FAllowVariable := True;
  FItemFormat := cfBoth;
end;

procedure TJvColorSpaceCombo.CreateWnd;
begin
  inherited CreateWnd;
  MakeList;
end;

function TJvColorSpaceCombo.GetColorSpace: TJvColorSpace;
begin
  if ItemIndex > -1 then
    Result := TJvColorSpace(Self.Items.Objects[ItemIndex])
  else
    Result := nil;
end;

function TJvColorSpaceCombo.GetColorSpaceID: TJvColorSpaceID;
var
  LColorSpace: TJvColorSpace;
begin
  LColorSpace := SelectedSpace;
  if LColorSpace <> nil then
    Result := LColorSpace.ID
  else
    Result := csRGB;
end;

procedure TJvColorSpaceCombo.MakeList;
var
  Index: Integer;
  LColorSpace: TJvColorSpace;
  OldColorID: TJvColorSpaceID;
begin
  OldColorID := ColorSpaceID;
  with ColorSpaceManager, Items do
  begin
    Clear;
    for Index := 0 to ColorSpaceManager.Count - 1 do
    begin
      LColorSpace := ColorSpaceByIndex[Index];
      if (LColorSpace.ID <> csDEF) or AllowVariable then
        AddObject(ColorSpaceToString(LColorSpace, ItemFormat), LColorSpace);
    end;
  end;
  ColorSpaceID := OldColorID;
end;

procedure TJvColorSpaceCombo.SetAllowVariable(const Value: Boolean);
begin
  if FAllowVariable <> Value then
  begin
    FAllowVariable := Value;
    MakeList;
  end;
end;

procedure TJvColorSpaceCombo.SetColorSpace(const Value: TJvColorSpace);
var
  Index: Integer;
begin
  with Items do
    for Index := 0 to Count - 1 do
      if Value.ID = TJvColorSpace(Objects[Index]).ID then
      begin
        ItemIndex := Index;
        Exit;
      end;
end;

procedure TJvColorSpaceCombo.SetColorSpaceID(const Value: TJvColorSpaceID);
begin
  SetColorSpace(ColorSpaceManager.ColorSpace[Value]);
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TJvColorSpaceCombo.SetItemFormat(const Value: TJvColorSpaceFormat);
begin
  if FItemFormat <> Value then
  begin
    FItemFormat := Value;
    MakeList;
  end;
end;

//=== { TJvColorAxisConfigCombo } ============================================

constructor TJvColorAxisConfigCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csDropDownList;
  FColorID := csRGB;
  FItemFormat := afComplete;
end;

procedure TJvColorAxisConfigCombo.CreateWnd;
begin
  inherited CreateWnd;
  MakeList;
end;

function TJvColorAxisConfigCombo.GetSelected: TJvColorAxisConfig;
begin
  Result := TJvColorAxisConfig(ItemIndex);
end;

procedure TJvColorAxisConfigCombo.MakeList;
var
  Index: TJvColorAxisConfig;
  LColorSpace: TJvColorSpace;
  OldItemIndex: Integer;
begin
  OldItemIndex := ItemIndex;
  LColorSpace := ColorSpaceManager.ColorSpace[ColorID];
  with Items do
  begin
    Clear;
    for Index := Low(TJvColorAxisConfig) to High(TJvColorAxisConfig) do
      Add(AxisConfigToString(Index, ItemFormat, LColorSpace));
  end;
  ItemIndex := OldItemIndex;
end;

procedure TJvColorAxisConfigCombo.SetColorID(const Value: TJvColorSpaceID);
begin
  if FColorID <> Value then
  begin
    FColorID := Value;
    MakeList;
  end;
end;

procedure TJvColorAxisConfigCombo.SetItemFormat(const Value: TJvColorAxisConfigFormat);
begin
  if FItemFormat <> Value then
  begin
    FItemFormat := Value;
    MakeList;
  end;
end;

procedure TJvColorAxisConfigCombo.SetSelected(const Value: TJvColorAxisConfig);
begin
  ItemIndex := Integer(Value);
end;

procedure Register;
begin
  RegisterComponents('Colors', [TJvColorPanel, TJvFullColorTrackBar, TJvColorLabel,
    TJvColorCircle, TJvColorSpaceCombo, TJvColorAxisConfigCombo]);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

