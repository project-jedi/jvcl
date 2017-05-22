{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ColorCtrls.pas, released on 2004-09-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFullColorCtrls;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Controls, Graphics,
  ComCtrls, StdCtrls, ExtCtrls, Types,
  JvJCLUtils, JvTypes, JvCombobox, JvFullColorSpaces, JvFullColorRotate;

type
  TJvFullColorAxisConfig = (acXYZ, acXZY, acYXZ, acYZX, acZXY, acZYX);
  TJvFullColorOrientation = (coNormal, coInverse);
  TJvArrowPosition = (apNormal, apOpposite);

type
  TJvKeyCode = (kcLeft, kcRight, kcUp, kcDown);

  TJvFullColorMouseEvent = procedure(Sender: TObject; ColorX, ColorY: Byte) of object;
  TJvFullColorComponent = class;
  TJvFullColorPanel = class;
  TJvFullColorCircle = class;
  TJvFullColorTrackBar = class;

  EJvFullColorError = class(EJVCLException);

  TJvFullColorComponent = class(TCustomControl)
  private
    FAutoMouse: Boolean;
    FFullColor: TJvFullColor;
    FAxisConfig: TJvFullColorAxisConfig;
    FOnColorChange: TNotifyEvent;
    FOnAxisConfigChange: TNotifyEvent;
    FOnColorSpaceChange: TNotifyEvent;
    FOnMouseColor: TJvFullColorMouseEvent;
    FColorChanging: Boolean;
    FBuffer: TBitmap;
    FCreating: Boolean;
    FWantDrawBuffer: Boolean;
    FMouseDragging: Boolean;
    function GetColorSpace: TJvColorSpace;
    procedure SetAxisConfig(const Value: TJvFullColorAxisConfig);
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
    procedure KeyMove(KeyCode: TJvKeyCode; MoveCount: Integer); virtual;
    procedure InvalidateCursor; virtual; abstract;
    property WantDrawBuffer: Boolean read FWantDrawBuffer write SetWantDrawBuffer;
    property MouseDragging: Boolean read FMouseDragging;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property ColorSpace: TJvColorSpace read GetColorSpace;
  published
    property AutoMouse: Boolean read FAutoMouse write FAutoMouse default True;
    property FullColor: TJvFullColor read FFullColor write SetFullColor;
    property AxisConfig: TJvFullColorAxisConfig read FAxisConfig write SetAxisConfig default acXYZ;
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
    property OnMouseColor: TJvFullColorMouseEvent read FOnMouseColor write FOnMouseColor;
  end;

  TJvFullColorComponent2D = class(TJvFullColorComponent)
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

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvFullColorPanel = class(TJvFullColorComponent2D)
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
    procedure KeyMove(KeyCode: TJvKeyCode; MoveCount: Integer); override;
    procedure InvalidateCursor; override;
    function GetCursorPosition: TPoint;
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

  TJvFullColorCircleStyle = (csShowLines, csShowCommon, csShowRed, csShowGreen,
    csShowBlue, cs3ButtonsMouse, cs3ButtonsCommon);

  TJvFullColorCircleStyles = set of TJvFullColorCircleStyle;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvFullColorCircle = class(TJvFullColorComponent2D)
  private
    FStyles: TJvFullColorCircleStyles;
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
    procedure SetStyles(const Value: TJvFullColorCircleStyles);
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
    procedure InvalidateColors(AColor1, AColor2: TJvFullColor);
    procedure PenChanged(Sender: TObject);
    procedure DrawBuffer; override;
    procedure CalcSize; override;
    procedure SetFullColor(const Value: TJvFullColor); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseColor(Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyMove(KeyCode: TJvKeyCode; MoveCount: Integer); override;
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
    property Styles: TJvFullColorCircleStyles read FStyles write SetStyles
      default [csShowLines, csShowRed, csShowGreen, csShowBlue];
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

  TJvCursorPoints = array [0..2] of TPoint;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvFullColorTrackBar = class(TJvFullColorComponent)
  private
    FArrowPosition: TJvArrowPosition;
    FColorOrientation: TJvFullColorOrientation;
    FOrientation: TTrackBarOrientation;
    FFullColorDrawing: Boolean;
    FArrowWidth: Integer;
    FArrowColor: TColor;
    FValueYAuto: Boolean;
    FValueXAuto: Boolean;
    FValueY: Byte;
    FValueX: Byte;
    FLink: TComponent;
    procedure SetArrowPosition(const Value: TJvArrowPosition);
    procedure SetColorOrientation(const Value: TJvFullColorOrientation);
    procedure SetOrientation(const Value: TTrackBarOrientation);
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
    procedure KeyMove(KeyCode: TJvKeyCode; MoveCount: Integer); override;
    procedure InvalidateCursor; override;
    procedure Paint; override;
    function GetCursorPosition: TJvCursorPoints;
  public
    constructor Create(AOwner: TComponent); override;
    function Linked: Boolean;
    function LinkerName: TComponentName;
    procedure SetLink(AComponent: TComponent);
    procedure FreeLink;
  published
    property ArrowColor: TColor read FArrowColor write SetArrowColor default clBlack;
    property ArrowWidth: Integer read FArrowWidth write SetArrowWidth default 9;
    property ArrowPosition: TJvArrowPosition read FArrowPosition write SetArrowPosition default apNormal;
    property ColorOrientation: TJvFullColorOrientation read FColorOrientation write SetColorOrientation
      default coNormal;
    property Orientation: TTrackBarOrientation read FOrientation write SetOrientation default trHorizontal;
    property ValueX: Byte read FValueX write SetValueX stored IsValueXStored;
    property ValueXAuto: Boolean read FValueXAuto write SetValueXAuto stored False;
    property ValueY: Byte read FValueY write SetValueY stored IsValueYStored;
    property ValueYAuto: Boolean read FValueYAuto write SetValueYAuto stored False;
    property FullColorDrawing: Boolean read FFullColorDrawing write SetFullColorDrawing default True;
  end;

  TJvShapePosition = (spLeft, spRight, spTop, spBottom);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvFullColorLabel = class(TGraphicControl)
  private
    FBrush: TBrush;
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
    procedure SetPen(const Value: TPen);
  protected
    procedure Paint; override;
    procedure CalcSize;
    procedure SetAutoSize(Value: Boolean); override;
    procedure GraphicChange(Sender: TObject);
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LabelColor: TJvFullColor read FLabelColor write SetLabelColor default fclDEFWindowText;
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
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

  TJvFullColorSpaceFormat = (cfName, cfShortName, cfBoth);

  TJvFullColorSpaceFormatEvent = procedure(Sender: TObject; AColorSpace: TJvColorSpace;
    out ACaption: string) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvFullColorSpaceCombo = class(TJvCustomComboBox)
  private
    FAllowVariable: Boolean;
    FItemFormat: TJvFullColorSpaceFormat;
    FOnFormatItem: TJvFullColorSpaceFormatEvent;
    function GetColorSpace: TJvColorSpace;
    procedure SetAllowVariable(const Value: Boolean);
    procedure SetColorSpace(const Value: TJvColorSpace);
    procedure SetColorSpaceID(const Value: TJvFullColorSpaceID);
    function GetColorSpaceID: TJvFullColorSpaceID;
    procedure SetItemFormat(const Value: TJvFullColorSpaceFormat);
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    property SelectedSpace: TJvColorSpace read GetColorSpace write SetColorSpace;
    procedure MakeList; virtual;
  published
    property AllowVariable: Boolean read FAllowVariable write SetAllowVariable default True;
    property ColorSpaceID: TJvFullColorSpaceID read GetColorSpaceID write SetColorSpaceID default csRGB;
    property ItemFormat: TJvFullColorSpaceFormat read FItemFormat write SetItemFormat default cfBoth;
    property OnFormatItem: TJvFullColorSpaceFormatEvent read FOnFormatItem write FOnFormatItem;
    property AutoDropDown;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Flat;
    property ParentFlat;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ParentBiDiMode;
    property ParentColor;
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

  TJvFullColorAxisConfigFormat = (afShort, afIndent, afComplete);

  TJvFullColorAxisFormatEvent = procedure(Sender: TObject; AAxisConfig: TJvFullColorAxisConfig;
    out ACaption: string) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvFullColorAxisCombo = class(TJvCustomComboBox)
  private
    FItemFormat: TJvFullColorAxisConfigFormat;
    FColorID: TJvFullColorSpaceID;
    FOnFormatItem: TJvFullColorAxisFormatEvent;
    procedure SetItemFormat(const Value: TJvFullColorAxisConfigFormat);
    procedure SetSelected(const Value: TJvFullColorAxisConfig);
    procedure SetColorID(const Value: TJvFullColorSpaceID);
    function GetSelected: TJvFullColorAxisConfig;
    procedure SetOnFormatItem(const Value: TJvFullColorAxisFormatEvent);
  protected
    procedure MakeList; virtual;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ItemFormat: TJvFullColorAxisConfigFormat read FItemFormat write SetItemFormat default afComplete;
    property Selected: TJvFullColorAxisConfig read GetSelected write SetSelected;
    property ColorID: TJvFullColorSpaceID read FColorID write SetColorID default csRGB;
    property OnFormatItem: TJvFullColorAxisFormatEvent read FOnFormatItem write SetOnFormatItem;
    property AutoDropDown;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Flat;
    property ParentFlat;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ParentBiDiMode;
    property ParentColor;
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

  TJvFullColorArray = array [0..{$IFDEF RTL230_UP}Maxint div 16{$ELSE}MaxListSize{$ENDIF RTL230_UP} - 1] of TJvFullColor;
  PJvFullColorArray = ^TJvFullColorArray;

  TJvFullColorListOperation = (foAllChanged, foDeleted, foAdded, foChanged);

  TJvFullColorListEvent = procedure(Sender: TObject; Index: Integer;
    Operation: TJvFullColorListOperation) of object;

  EJvFullColorListError = class(Exception);

  TJvFullColorList = class(TPersistent)
  private
    FCapacity: Integer;
    FCount: Integer;
    FList: PJvFullColorArray;
    FOnChange: TJvFullColorListEvent;
    FUpdateCount: Integer;
    FAllocBy: Integer;
    procedure SetCapacity(const Value: Integer);
    procedure SetCount(const Value: Integer);
    procedure SetAllocBy(const Value: Integer);
  protected
    procedure Grow;
    function GetItem(Index: Integer): TJvFullColor;
    procedure SetItem(Index: Integer; const Value: TJvFullColor);
    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteItems(Writer: TWriter);
    procedure ReadItems(Reader: TReader);
    procedure Change(AIndex: Integer; AOperation: TJvFullColorListOperation);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AColor: TJvFullColor): Integer;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function Remove(AColor: TJvFullColor): Integer;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Insert(Index: Integer; AColor: TJvFullColor);
    function IndexOf(AColor: TJvFullColor): Integer;
    procedure DeleteRedundant;
    procedure BeginUpdate;
    procedure EndUpdate;
    property AllocBy: Integer read FAllocBy write SetAllocBy;
    property Items[Index: Integer]: TJvFullColor read GetItem write SetItem; default;
    property List: PJvFullColorArray read FList;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property UpdateCount: Integer read FUpdateCount;
    property OnChange: TJvFullColorListEvent read FOnChange write FOnChange;
  end;

  TJvFullColorEdge = (feRaised, feLowered, feFlat);
  TJvFormatHintEvent = procedure(Sender: TObject; HintColor: TJvFullColor;
    var HintText: string) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvFullColorGroup = class(TCustomControl)
  private
    FItems: TJvFullColorList;
    FColCount: Integer;
    FEdge: TJvFullColorEdge;
    FSelectedEdge: TJvFullColorEdge;
    FMouseEdge: TJvFullColorEdge;
    FSquareSize: Integer;
    FMouseIndex: Integer;
    FSelectedIndex: Integer;
    FBrush: TBrush;
    FOnChange: TNotifyEvent;
    FOnFormatHint: TJvFormatHintEvent;
    procedure SetItems(const Value: TJvFullColorList);
    procedure SetColCount(const Value: Integer);
    function GetRowCount: Integer;
    procedure SetEdge(const Value: TJvFullColorEdge);
    procedure SetMouseEdge(const Value: TJvFullColorEdge);
    procedure SetSelectedEdge(const Value: TJvFullColorEdge);
    procedure SetSquareSize(const Value: Integer);
    function GetSelected: TJvFullColor;
    procedure SetSelected(const Value: TJvFullColor);
    procedure SetSelectedIndex(const Value: Integer);
    procedure SetBrush(const Value: TBrush);
    procedure MouseLeave(var Msg: TWMMouse); message WM_MOUSELEAVE;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
  protected
    procedure Paint; override;
    procedure ItemsChange(Sender: TObject; Index: Integer;
      Operation: TJvFullColorListOperation);
    procedure BrushChange(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CalcRects(out XPos, YPos, XInc, YInc: Integer);
    procedure InvalidateIndex(AIndex: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MouseIndex: Integer read FMouseIndex;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property Selected: TJvFullColor read GetSelected write SetSelected;
    property RowCount: Integer read GetRowCount;
  published
    property Items: TJvFullColorList read FItems write SetItems;
    property ColCount: Integer read FColCount write SetColCount default 4;
    property Edge: TJvFullColorEdge read FEdge write SetEdge default feRaised;
    property SelectedEdge: TJvFullColorEdge read FSelectedEdge write SetSelectedEdge default feLowered;
    property MouseEdge: TJvFullColorEdge read FMouseEdge write SetMouseEdge default feRaised;
    property SquareSize: Integer read FSquareSize write SetSquareSize default 6;
    property Brush: TBrush read FBrush write SetBrush;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnFormatHint: TJvFormatHintEvent read FOnFormatHint write FOnFormatHint;
    property Align;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelEdges;
    property BevelKind default bkTile;
    property BevelWidth;
    property BorderWidth;
    property Color;
    property Constraints;
    property Hint;
    property ParentShowHint;
    property ParentColor;
    property ShowHint;
    property Visible;
  end;

function GetIndexAxis(AxisConfig: TJvFullColorAxisConfig; AxisID: TJvAxisIndex): TJvAxisIndex;
function GetIndexAxisX(AxisConfig: TJvFullColorAxisConfig): TJvAxisIndex;
function GetIndexAxisY(AxisConfig: TJvFullColorAxisConfig): TJvAxisIndex;
function GetIndexAxisZ(AxisConfig: TJvFullColorAxisConfig): TJvAxisIndex;
function ColorSpaceToString(AColorSpace: TJvColorSpace;
  ItemFormat: TJvFullColorSpaceFormat): string;
function AxisConfigToString(AxisConfig: TJvFullColorAxisConfig;
  ItemFormat: TJvFullColorAxisConfigFormat; AColorSpace: TJvColorSpace): string;

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
  RTLConsts, TypInfo, Forms,
  JclMath, JclLogic, // For EnsureRange and Min/Max
  JvResources, JvConsts, JvJVCLUtils;

type
  TJvFullColorAxisConfigs = array [TJvAxisIndex] of TJvAxisIndex;

const
  TabAxisConfigs: array [TJvFullColorAxisConfig] of TJvFullColorAxisConfigs =
   ((axIndex0, axIndex1, axIndex2),
    (axIndex0, axIndex2, axIndex1),
    (axIndex1, axIndex0, axIndex2),
    (axIndex2, axIndex0, axIndex1),
    (axIndex1, axIndex2, axIndex0),
    (axIndex2, axIndex1, axIndex0));

function ColorSpaceToString(AColorSpace: TJvColorSpace; ItemFormat: TJvFullColorSpaceFormat): string;
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

function AxisConfigToString(AxisConfig: TJvFullColorAxisConfig;
  ItemFormat: TJvFullColorAxisConfigFormat; AColorSpace: TJvColorSpace): string;
var
  Str: string;
  AxisConfigs: TJvFullColorAxisConfigs;
begin
  Str := GetEnumName(TypeInfo(TJvFullColorAxisConfig), Ord(AxisConfig));
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

function GetIndexAxis(AxisConfig: TJvFullColorAxisConfig; AxisID: TJvAxisIndex): TJvAxisIndex;
begin
  Result := TabAxisConfigs[AxisConfig][AxisID];
end;

function GetIndexAxisX(AxisConfig: TJvFullColorAxisConfig): TJvAxisIndex;
begin
  Result := TabAxisConfigs[AxisConfig][axIndex0];
end;

function GetIndexAxisY(AxisConfig: TJvFullColorAxisConfig): TJvAxisIndex;
begin
  Result := TabAxisConfigs[AxisConfig][axIndex1];
end;

function GetIndexAxisZ(AxisConfig: TJvFullColorAxisConfig): TJvAxisIndex;
begin
  Result := TabAxisConfigs[AxisConfig][axIndex2];
end;

//=== { TJvColorComponent } ==================================================

constructor TJvFullColorComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBuffer := TBitmap.Create;
  FBuffer.PixelFormat := pf32Bit;
  FAutoMouse := True;
  FAxisConfig := acXYZ;
  FFullColor := fclRGBWhite;

  TabStop := True;
  ControlStyle := [csSetCaption, csOpaque];
  Width := 100;
  Height := 100;
end;

destructor TJvFullColorComponent.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

procedure TJvFullColorComponent.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  CalcSize;
end;

procedure TJvFullColorComponent.CalcSize;
begin
  WantDrawBuffer := True;
end;

procedure TJvFullColorComponent.DrawBuffer;
begin
  Invalidate;
end;

procedure TJvFullColorComponent.Paint;
begin
  if WantDrawBuffer then
    DrawBuffer;
  WantDrawBuffer := False;
  inherited Paint;
end;

procedure TJvFullColorComponent.DrawFocus;
begin
  if Focused and not (csDesigning in ComponentState) then
    with Canvas do
    begin
      Pen.Color := Color;
      Brush.Color := Color;
      DrawFocusRect(ClientRect);
    end;
end;

procedure TJvFullColorComponent.DrawFrame(X, Y: Integer);
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0, 0, Width, Y));
  Canvas.FillRect(Rect(0, Y + FBuffer.Height, Width, Height));
  Canvas.FillRect(Rect(0, Y, X, Y + FBuffer.Height));
  Canvas.FillRect(Rect(X + FBuffer.Width, Y, Width, Y + FBuffer.Height));
end;

procedure TJvFullColorComponent.SetFullColor(const Value: TJvFullColor);
var
  NewColorID: TJvFullColorSpaceID;
  OldColorID: TJvFullColorSpaceID;
  OldColor: TJvFullColor;
begin
  if Value <> FullColor then
  begin
    OldColor := FFullColor;
    NewColorID := ColorSpaceManager.GetColorSpaceID(Value);
    if NewColorID = csDEF then
      raise EJvFullColorError.CreateResFmt(@RsEUnsupportedColorSpace, [NewColorID]);
    OldColorID := ColorSpaceManager.GetColorSpaceID(OldColor);
    FFullColor := Value;
    if OldColorID <> ColorSpaceManager.GetColorSpaceID(FFullColor) then
      ColorSpaceChange;

    if Assigned(FOnColorChange) then
      FOnColorChange(Self);
  end;
end;

procedure TJvFullColorComponent.MouseColor(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseColor) then
    FOnMouseColor(Self, X, Y);
end;

procedure TJvFullColorComponent.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SetFocus;
  try
    if AutoMouse and (Shift * [ssLeft, ssMiddle, ssRight] <> []) then
    begin
      FMouseDragging := True;
      MouseColor(Shift, X, Y);
    end;
    inherited MouseDown(Button, Shift, X, Y);
  finally
    SetCapture(Handle);
  end;
end;

procedure TJvFullColorComponent.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if MouseDragging and AutoMouse and (Shift * [ssLeft, ssMiddle, ssRight] <> []) then
    MouseColor(Shift, X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvFullColorComponent.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  try
    FMouseDragging := False;
    inherited MouseUp(Button, Shift, X, Y);
  finally
    ReleaseCapture;
  end;
end;

procedure TJvFullColorComponent.SetAxisConfig(const Value: TJvFullColorAxisConfig);
begin
  if FAxisConfig <> Value then
  begin
    FAxisConfig := Value;
    AxisConfigChange;
  end;
end;

procedure TJvFullColorComponent.ColorSpaceChange;
begin
  CalcSize;
  if Assigned(FOnColorSpaceChange) then
    FOnColorSpaceChange(Self);
end;

function TJvFullColorComponent.GetColorSpace: TJvColorSpace;
begin
  with ColorSpaceManager do
    Result := ColorSpace[GetColorSpaceID(FullColor)];
end;

procedure TJvFullColorComponent.AxisConfigChange;
begin
  CalcSize;
  if Assigned(FOnAxisConfigChange) then
    FOnAxisConfigChange(Self);
end;

procedure TJvFullColorComponent.SetWantDrawBuffer(Value: Boolean);
begin
  FWantDrawBuffer := Value;
  if Value and (Width <> 0) and (Height <> 0) then
    Invalidate;
end;

procedure TJvFullColorComponent.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TJvFullColorComponent.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;

procedure TJvFullColorComponent.DoExit;
begin
  inherited DoExit;
  Invalidate;
end;

procedure TJvFullColorComponent.KeyMove(KeyCode: TJvKeyCode; MoveCount: Integer);
begin
  Invalidate;
end;

procedure TJvFullColorComponent.KeyDown(var Key: Word; Shift: TShiftState);
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

procedure TJvFullColorComponent.CMColorChanged(var Msg: TMessage);
begin
  inherited;
  WantDrawBuffer := True;
end;

procedure TJvFullColorComponent.CMSysColorChange(var Msg: TMessage);
begin
  inherited;
  WantDrawBuffer := True;
end;

//=== { TColor2D } ===========================================================

constructor TJvFullColorComponent2D.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValueZ := 0;
  FValueZAuto := True;
  ColorSpaceChange;
end;

procedure TJvFullColorComponent2D.AxisConfigChange;
begin
  UpdateDefaultValueZ;
  inherited AxisConfigChange;
end;

procedure TJvFullColorComponent2D.ColorSpaceChange;
begin
  UpdateDefaultValueZ;
  inherited ColorSpaceChange;
end;

procedure TJvFullColorComponent2D.TrackBarAxisConfigChange(Sender: TObject);
begin
  if not FAxisConfigChanging then
  begin
    FAxisConfigChanging := True;
    AxisConfig := (Sender as TJvFullColorTrackBar).AxisConfig;
    FAxisConfigChanging := False;
  end;
end;

procedure TJvFullColorComponent2D.TrackBarColorChange(Sender: TObject);
begin
  if FColorChanging then
    Exit;

  FColorChanging := True;
  FullColor := (Sender as TJvFullColorTrackBar).FullColor;
  FColorChanging := False;

  if Assigned(FOnColorChange) then
    FOnColorChange(Self);
end;

function TJvFullColorComponent2D.IsValueZStored: Boolean;
begin
  Result := not ValueZAuto;
end;

procedure TJvFullColorComponent2D.SetValueZ(const Value: Byte);
begin
  FValueZAuto := False;
  FValueZ := Value;
  WantDrawBuffer := True;
end;

procedure TJvFullColorComponent2D.SetValueZAuto(const Value: Boolean);
begin
  FValueZAuto := Value;
  if Value then
    UpdateDefaultValueZ;
  WantDrawBuffer := True;
end;

procedure TJvFullColorComponent2D.UpdateDefaultValueZ;
begin
  if ValueZAuto then
    FValueZ := ColorSpace.AxisDefault[GetIndexAxisZ(AxisConfig)];
end;

//=== { TJvColorPanel } ======================================================

constructor TJvFullColorPanel.Create(AOwner: TComponent);
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

destructor TJvFullColorPanel.Destroy;
begin
  ColorTrackBar := nil;
  FPen.Free;
  inherited Destroy;
end;

procedure TJvFullColorPanel.CalcSize;
begin
  FBuffer.Width := Max(Width - 2 * FCrossSize,0);
  FBuffer.Height := Max(Height - 2 * FCrossSize,0);
  inherited CalcSize;
end;

procedure TJvFullColorPanel.DrawBuffer;
var
  AxisX, AxisY: TJvAxisIndex;
  IndexX, IndexY: Integer;
  MinX, MaxX, MinY, MaxY: Integer;
  RangeX, RangeY: Integer;
  TempColor: TJvFullColor;
  Line: PJvFullColorArray;
begin
  if (FBuffer.Width = 0) or (FBuffer.Height = 0) or (Width = 0) or (Height = 0) then
    Exit;

  AxisX := GetIndexAxisX(AxisConfig);
  AxisY := GetIndexAxisY(AxisConfig);

  with ColorSpace do
  begin
    MinX := AxisMin[AxisX];
    MaxX := AxisMax[AxisX];
    RangeX := MaxX - MinX;
    MinY := AxisMin[AxisY];
    MaxY := AxisMax[AxisY];
    RangeY := MaxY - MinY;


    TempColor := SetAxisValue(fclRGBBlack, GetIndexAxisZ(AxisConfig), ValueZ);
    with FBuffer do
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(Rect(0, 0, Width-1, Height-1));
      for IndexY := 0 to Height-1 do
      begin
        Line := ScanLine[IndexY];
        if ReverseAxisY then
          TempColor := SetAxisValue(TempColor, AxisY, MaxY - (RangeY * IndexY) div (Height - 1))
        else
          TempColor := SetAxisValue(TempColor, AxisY, (RangeY * IndexY) div (Height - 1) + MinY);
        for IndexX := 0 to Width-1 do
        begin
          if ReverseAxisX then
            TempColor := SetAxisValue(TempColor, AxisX, MaxX - (RangeX * IndexX) div (Width - 1))
          else
            TempColor := SetAxisValue(TempColor, AxisX, (RangeX * IndexX) div (Width - 1) + MinX);
          // (outchy) don't remove, Bitmap colors are stocked as (MSB) 00RRGGBB (LSB)
          // Delphi TColor is (MSB) 00BBGGRR (LSB)
          Line[IndexX] := RGBToBGR(ConvertToColor(TempColor));
        end;
      end;
    end;
  end;
  inherited DrawBuffer;
end;

function TJvFullColorPanel.GetCursorPosition: TPoint;
var
  AxisX, AxisY: TJvAxisIndex;
  MinAxis, MaxAxis: Integer;
begin
  if (FBuffer.Width = 0) or (FBuffer.Height = 0) or (Width = 0) or (Height = 0) then
  begin
    Result.X := 0;
    Result.Y := 0;
  end
  else
    with ColorSpaceManager, ColorSpace[GetColorSpaceID(FullColor)] do
    begin
      AxisX := GetIndexAxisX(AxisConfig);
      MinAxis := AxisMin[AxisX];
      MaxAxis := AxisMax[AxisX];
      Result.X := GetAxisValue(FullColor, AxisX);
      if ReverseAxisX then
        Result.X := MaxAxis - Result.X
      else
        Result.X := Result.X - MinAxis;
      Result.X := ((Result.X * (FBuffer.Width - 1)) div (MaxAxis-MinAxis)) + CrossSize;

      AxisY := GetIndexAxisY(AxisConfig);
      MinAxis := AxisMin[AxisY];
      MaxAxis := AxisMax[AxisY];
      Result.Y := GetAxisValue(FullColor, AxisY);
      if ReverseAxisY then
        Result.Y := MaxAxis - Result.Y
      else
        Result.Y := Result.Y - MinAxis;
      Result.Y := ((Result.Y * (FBuffer.Height - 1)) div (MaxAxis-MinAxis)) + CrossSize;
    end;
end;

procedure TJvFullColorPanel.InvalidateCursor;
var
  ARect: TRect;
  Pt: TPoint;
begin
  Pt := GetCursorPosition;
  ARect.Left := Pt.X - 1 - CrossSize - CrossStyle.Width;
  ARect.Right := Pt.X + 1 + CrossSize + CrossStyle.Width;
  ARect.Top := Pt.Y - 1 - CrossSize - CrossStyle.Width;
  ARect.Bottom := Pt.Y + 1 + CrossSize + CrossStyle.Width;
  Windows.InvalidateRect(Handle, @ARect, False);
end;

procedure TJvFullColorPanel.Paint;
var
  Pt: TPoint;
begin
  inherited Paint;

  with Canvas do
  begin
    Brush.Color := Color;
    DrawFrame(CrossSize, CrossSize);
    Draw(CrossSize, CrossSize, FBuffer);
    Pen := CrossStyle;

    Pt := GetCursorPosition;
    MoveTo(Pt.X - CrossSize, Pt.Y);
    LineTo(Pt.X - CrossCenter, Pt.Y);
    MoveTo(Pt.X + CrossCenter, Pt.Y);
    LineTo(Pt.X + CrossSize, Pt.Y);

    MoveTo(Pt.X, Pt.Y - CrossSize);
    LineTo(Pt.X, Pt.Y - CrossCenter);
    MoveTo(Pt.X, Pt.Y + CrossCenter);
    LineTo(Pt.X, Pt.Y + CrossSize);
  end;
  DrawFocus;
end;

procedure TJvFullColorPanel.PenChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvFullColorPanel.SetCrossCenter(Value: Integer);
begin
  if Value >= CrossSize then
    Value := CrossSize - 1;
  if FCrossCenter <> Value then
  begin
    FCrossCenter := Value;
    Invalidate;
  end;
end;

procedure TJvFullColorPanel.SetCrossSize(Value: Integer);
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

procedure TJvFullColorPanel.SetReverseAxisX(const Value: Boolean);
begin
  if FReverseAxisX <> Value then
  begin
    FReverseAxisX := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvFullColorPanel.SetReverseAxisY(const Value: Boolean);
begin
  if FReverseAxisY <> Value then
  begin
    FReverseAxisY := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvFullColorPanel.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
  Invalidate;
end;

procedure TJvFullColorPanel.SetColorTrackBar(const Value: TJvFullColorTrackBar);
begin
  if (Value <> nil) and (Value <> FColorTrackBar) and Value.Linked then
    raise EJvFullColorError.CreateResFmt(@RsEDuplicateTrackBar, [Value.LinkerName]);

  if Assigned(FColorTrackBar) then
  begin
    FColorTrackBar.OnColorChange := nil;
    FColorTrackBar.OnAxisConfigChange := nil;
    FColorTrackBar.FreeLink;
  end;

  ReplaceComponentReference(Self, Value, TComponent(FColorTrackBar));

  if Assigned(FColorTrackBar) then
  begin
    FColorTrackBar.OnColorChange := TrackBarColorChange;
    FColorTrackBar.OnAxisConfigChange := TrackBarAxisConfigChange;
    FColorTrackBar.FullColor := FullColor;
    FColorTrackBar.AxisConfig := AxisConfig;
    FColorTrackBar.SetLink(Self);
  end;
end;

procedure TJvFullColorPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = ColorTrackBar) then
    ColorTrackBar := nil;
end;

procedure TJvFullColorPanel.SetFullColor(const Value: TJvFullColor);
var
  AxisX, AxisY: TJvAxisIndex;
begin
  if Value <> FullColor then
  begin
    if Assigned(FColorTrackBar) and (not FColorChanging) then
    begin
      FColorChanging := True;
      FColorTrackBar.FullColor := Value;
      FColorChanging := False;
    end;
    begin
      AxisX := GetIndexAxisX(AxisConfig);
      AxisY := GetIndexAxisY(AxisConfig);
      if (GetAxisValue(Value, AxisX) <> GetAxisValue(FullColor, AxisX)) or
        (GetAxisValue(Value, AxisY) <> GetAxisValue(FullColor, AxisY)) then
      begin
        InvalidateCursor;
        inherited SetFullColor(Value);
        InvalidateCursor;
      end
      else
        inherited SetFullColor(Value);
    end;
  end;
end;

procedure TJvFullColorPanel.MouseColor(Shift: TShiftState; X, Y: Integer);
var
  MinX, MaxX, MinY, MaxY: Byte;
  AxisX, AxisY: TJvAxisIndex;
  PosX, PosY: Integer;
begin
  if ssLeft in Shift then
  begin
    AxisX := GetIndexAxisX(AxisConfig);
    AxisY := GetIndexAxisY(AxisConfig);
    with ColorSpace do
    begin
      MinX := AxisMin[AxisX];
      MaxX := AxisMax[AxisX];
      MinY := AxisMin[AxisY];
      MaxY := AxisMax[AxisY];

      PosX := EnsureRange(((X - CrossSize) * (MaxX - MinX)) div (FBuffer.Width - 1), 0, MaxX - MinX);
      if ReverseAxisX then
        PosX := MaxX - PosX
      else
        PosX := PosX + MinX;

      PosY := EnsureRange(((Y - CrossSize) * (MaxY - MinY)) div (FBuffer.Height - 1), 0, MaxY - MinY);
      if ReverseAxisY then
        PosY := MaxY - PosY
      else
        PosY := PosY + MinY;

      FullColor := SetAxisValue(SetAxisValue(FullColor, AxisX, Byte(PosX)), AxisY, Byte(PosY));
    end;
  end;
  inherited MouseColor(Shift, X, Y);
end;

procedure TJvFullColorPanel.AxisConfigChange;
begin
  if (FColorTrackBar <> nil) and not FAxisConfigChanging then
  begin
    FAxisConfigChanging := True;
    FColorTrackBar.AxisConfig := AxisConfig;
    FAxisConfigChanging := False;
  end;
  inherited AxisConfigChange;
end;

procedure TJvFullColorPanel.KeyMove(KeyCode: TJvKeyCode; MoveCount: Integer);
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

constructor TJvFullColorCircle.Create(AOwner: TComponent);
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
  FStyles := [csShowLines, csShowRed, csShowGreen, csShowBlue];
end;

destructor TJvFullColorCircle.Destroy;
begin
  FCrossStyle.Free;
  inherited Destroy;
end;

procedure TJvFullColorCircle.CalcSize;
begin
  FBuffer.Width := Max(Width - (2 * CrossSize),0);
  FBuffer.Height := Max(Height - (2 * CrossSize),0);
  inherited CalcSize;
end;

procedure TJvFullColorCircle.DrawBuffer;
var
  X, Y, Angle, RadiusInt, MaxRadius, MinRadius: Integer;
  AxisRadius, AxisAngle: TJvAxisIndex;
  MaxAngle, MinAngle: Integer;
  AngleUnit, AngleUnitPi, XCenter, YCenter, XRelative, YRelative,
  SqrXRelative, SqrYRelative, Radius: Extended;
  Magic1, Magic2, Magic3: Byte;
  Line: PJvFullColorArray;
begin
  if (FBuffer.Width = 0) or (FBuffer.Height = 0) then
    Exit;

  AxisRadius := GetIndexAxisX(AxisConfig);
  AxisAngle := GetIndexAxisY(AxisConfig);

  with ColorSpace do
  begin
    MaxRadius := AxisMax[AxisRadius];
    MinRadius := AxisMin[AxisRadius];
    MaxAngle := AxisMax[AxisAngle];
    MinAngle := AxisMin[AxisAngle];
  end;

  AngleUnit := (MaxAngle - MinAngle) / 2.0 / Pi;
  AngleUnitPi := (MaxAngle - MinAngle) / 2.0;

  Magic1 := ValueZ;
  Magic2 := Magic1;
  Magic3 := Magic1;

  with FBuffer, ColorSpace do
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    XCenter := Width / 2.0;
    YCenter := Height / 2.0;
    for Y := 0 to Height - 1 do
    begin
      Line := ScanLine[Y];
      YRelative := Y - YCenter;
      SqrYRelative := Sqr(YRelative / YCenter);
      for X := 0 to Width - 1 do
      begin
        XRelative := X - XCenter;
        SqrXRelative := Sqr(XRelative / XCenter);
        Radius := Sqrt(SqrYRelative + SqrXRelative);

        if Radius <= 1.0 then
        begin
          Angle := Round(ArcTan2(YRelative, XRelative) * AngleUnit + AngleUnitPi) + MinAngle;
          RadiusInt := Round(Radius * (MaxRadius - MinRadius));
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
                Magic1 := MaxRadius - RadiusInt
              else
                Magic1 := RadiusInt + MinRadius;
            axIndex1:
              if InvertRadius then
                Magic2 := MaxRadius - RadiusInt
              else
                Magic2 := RadiusInt + MinRadius;
            axIndex2:
              if InvertRadius then
                Magic3 := MaxRadius - RadiusInt
              else
                Magic3 := RadiusInt + MinRadius;
          end;
          // (outchy) don't remove, Bitmap colors are stocked as (MSB) 00RRGGBB (LSB)
          // Delphi TColor is (MSB) 00BBGGRR (LSB)
          Line[X] := RGBToBGR(ConvertToColor(Magic1 or (Magic2 shl 8) or (Magic3 shl 16)));
        end
        else
        if XRelative >= 0.0 then
          Break;         // end of a line
      end;
    end;
  end;
  inherited DrawBuffer;
end;

procedure TJvFullColorCircle.Paint;

  procedure DrawCross(AFullColor: TJvFullColor; ACrossColor: TColor);
  var
    Point: TPoint;
  begin
    Point := FullColorToPosition(AFullColor);

    with Canvas do
    begin
      Pen := CrossStyle;
      Pen.Color := ACrossColor;

      MoveTo(Point.X - CrossSize, Point.Y);     // left
      LineTo(Point.X - CrossCenter, Point.Y);

      MoveTo(Point.X + CrossCenter, Point.Y);   // right
      LineTo(Point.X + CrossSize, Point.Y);

      MoveTo(Point.X, Point.Y - CrossSize);     // top
      LineTo(Point.X, Point.Y - CrossCenter);

      MoveTo(Point.X, Point.Y + CrossCenter);   // bottom
      LineTo(Point.X, Point.Y + CrossSize);

      Pen.Mode := pmCopy;
      Pen.Style := psSolid;
      Pen.Width := LineWidth;
      MoveTo((FBuffer.Width div 2) + CrossSize + 1,(FBuffer.Height div 2 ) + CrossSize + 1);
      LineTo(Point.X, Point.Y);
    end;
  end;

begin
  inherited Paint;
  with Canvas do
  begin
    Brush.Color := Color;
    DrawFrame(CrossSize, CrossSize);
    Draw(CrossSize, CrossSize, FBuffer);

    if csShowCommon in Styles then
      DrawCross(FullColor, CrossStyle.Color)
    else
    begin
      if csShowBlue in Styles then
        DrawCross(BlueColor, CrossBlueColor);
      if csShowRed in Styles then
        DrawCross(RedColor, CrossRedColor);
      if csShowGreen in Styles then
        DrawCross(GreenColor, CrossGreenColor);
    end;
  end;
  DrawFocus;
end;

function TJvFullColorCircle.FullColorToPosition(AFullColor: TJvFullColor): TPoint;
var
  ColorID: TJvFullColorSpaceID;
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

  FullAngle := (2 * Pi * Angle) / (AngleMax - AngleMin) - Pi;
  Result.X := Round(Radius * JclMath.Cos(FullAngle) * FBuffer.Width / (Radius1 * 2) + (FBuffer.Width / 2.0)) + CrossSize;
  Result.Y := Round(Radius * JclMath.Sin(FullAngle) * FBuffer.Height / (Radius1 * 2) + (FBuffer.Height / 2.0)) + CrossSize;
end;

function TJvFullColorCircle.PositionToFullColor(APoint: TPoint): TJvFullColor;
var
  RadiusIndex, AngleIndex: TJvAxisIndex;
  Radius, RadiusMax, RadiusMin, Angle, AngleMax, AngleMin: Integer;
  XPos, YPos: Extended;
begin
  if (FBuffer.Width = 0) or (FBuffer.Height = 0) then
  begin
    Result := fclRGBBlack;
    Exit;
  end;
  with ColorSpace do
  begin
    RadiusIndex := GetIndexAxisX(AxisConfig);
    RadiusMax := AxisMax[RadiusIndex];
    RadiusMin := AxisMin[RadiusIndex];

    AngleIndex := GetIndexAxisY(AxisConfig);
    AngleMax := AxisMax[AngleIndex];
    AngleMin := AxisMin[AngleIndex];
  end;

  XPos := FBuffer.Width / 2.0;
  XPos := (APoint.X - CrossSize - XPos) / XPos;
  YPos := FBuffer.Height / 2.0;
  YPos := (APoint.Y - CrossSize - YPos) / YPos;

  Radius := Round(Sqrt(Sqr(XPos) + Sqr(YPos))*(RadiusMax - RadiusMin));
  Angle := Round((ArcTan2(YPos, XPos) + Pi) * (AngleMax - AngleMin) / 2.0 / Pi);

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

procedure TJvFullColorCircle.MouseColor(Shift: TShiftState; X, Y: Integer);
var
  LFullColor: TJvFullColor;

  function MoveColor(var AFullColor: TJvFullColor): Boolean;
  var
    Distance: Integer;
    Point: TPoint;
  begin
    Point := FullColorToPosition(AFullColor);
    Distance := Round(Sqrt(Sqr(X - Point.X) + Sqr(Y - Point.Y)));
    if Distance < CrossSize then
    begin
      AFullColor := LFullColor;
      Result := True;
      Invalidate;
    end
    else
      Result := False;
  end;

begin
  LFullColor := PositionToFullColor(Point(X, Y));
  if csShowCommon in Styles then
  begin
    if (ssLeft in Shift) or
      ((cs3ButtonsMouse in Styles) and (cs3ButtonsCommon in Styles)) then
      FullColor := LFullColor;
  end
  else
  if cs3ButtonsMouse in Styles then
  begin
    if (ssLeft in Shift) and (csShowRed in Styles) then
      RedColor := LFullColor;
    if (ssMiddle in Shift) and (csShowGreen in Styles) then
      GreenColor := LFullColor;
    if (ssRight in Shift) and (csShowBlue in Styles) then
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
      if (csShowGreen in Styles) and MoveColor(FGreenColor) then
      begin
        FDraggingColor := rcGreen;
        if Assigned(FOnGreenColorChange) then
          FOnGreenColorChange(Self);
      end
      else
      if (csShowRed in Styles) and MoveColor(FRedColor) then
      begin
        FDraggingColor := rcRed;
        if Assigned(FOnRedColorChange) then
          FOnRedColorChange(Self);
      end
      else
      if (csShowBlue in Styles) and MoveColor(FBlueColor) then
      begin
        FDraggingColor := rcBlue;
        if Assigned(FOnBlueColorChange) then
          FOnBlueColorChange(Self);
      end;
    end;
  end;
end;

procedure TJvFullColorCircle.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FDraggingColor := rcCommon;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvFullColorCircle.KeyMove(KeyCode: TJvKeyCode;
  MoveCount: Integer);
begin
  // (outchy) todo implementation but how to select a cursor ???
end;

procedure TJvFullColorCircle.PenChanged(Sender: TObject);
begin
  WantDrawBuffer := True;
end;

procedure TJvFullColorCircle.ConvertToID(NewFullColor: TJvFullColor);
var
  ColorID: TJvFullColorSpaceID;
  Change: Boolean;
begin
  with ColorSpaceManager do
  begin
    ColorID := GetColorSpaceID(NewFullColor);
    Change := ColorID <> GetColorSpaceID(FullColor);

    if Change then
    begin
      FFullColor := ConvertToID(FullColor, ColorID);
      FRedColor := ConvertToID(RedColor, ColorID);
      FGreenColor := ConvertToID(GreenColor, ColorID);
      FBlueColor := ConvertToID(BlueColor, ColorID);
      ColorSpaceChange;
    end;
  end;
end;

procedure TJvFullColorCircle.InvalidateColors(AColor1, AColor2: TJvFullColor);
var
  AxisX, AxisY: TJvAxisIndex;
  APosition1,
    APosition2: TPoint;
  ARect: TRect;
  CenterX, CenterY: Integer;
begin
  AxisX := GetIndexAxisX(AxisConfig);
  AxisY := GetIndexAxisY(AxisConfig);

  if (GetAxisValue(AColor1, AxisX) <> GetAxisValue(AColor2, AxisX)) or
    (GetAxisValue(AColor1, AxisY) <> GetAxisValue(AColor2, AxisY)) then
  begin
    APosition1 := FullColorToPosition(AColor1);
    APosition2 := FullColorToPosition(AColor2);
    if APosition1.X < APosition2.X then
    begin
      ARect.Left := APosition1.X;
      ARect.Right := APosition2.X;
    end
    else
    begin
      ARect.Left := APosition2.X;
      ARect.Right := APosition1.X;
    end;
    if APosition1.Y < APosition2.Y then
    begin
      ARect.Top := APosition1.Y;
      ARect.Bottom := APosition2.Y;
    end
    else
    begin
      ARect.Top := APosition2.Y;
      ARect.Bottom := APosition1.Y;
    end;

    CenterX := Width div 2;
    CenterY := Height div 2;
    if ARect.Left > CenterX then
      ARect.Left := CenterX;
    if ARect.Top > CenterY then
      ARect.Top := CenterY;
    if ARect.Right < CenterX then
      ARect.Right := CenterX;
    if ARect.Bottom < CenterY then
      ARect.Bottom := CenterY;

    ARect.Left := ARect.Left - CrossStyle.Width - CrossSize;
    ARect.Top := ARect.Top - CrossStyle.Width - CrossSize;
    ARect.Right := ARect.Right + CrossStyle.Width + CrossSize;
    ARect.Bottom := ARect.Bottom + CrossStyle.Width + CrossSize;

    Windows.InvalidateRect(Handle, @ARect, False);
  end;
end;

procedure TJvFullColorCircle.SetFullColor(const Value: TJvFullColor);
var
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

  InvalidateColors(OldColor, FullColor);

  if ColorSpaceManager.GetColorSpaceID(OldColor) <> ColorSpaceManager.GetColorSpaceID(FullColor) then
    CalcSize;
end;

procedure TJvFullColorCircle.SetBlueColor(const Value: TJvFullColor);
var
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

  InvalidateColors(OldColor, BlueColor);

  if Assigned(FOnBlueColorChange) then
    FOnBlueColorChange(Self);
end;

procedure TJvFullColorCircle.SetGreenColor(const Value: TJvFullColor);
var
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

  InvalidateColors(OldColor, GreenColor);

  if Assigned(FOnGreenColorChange) then
    FOnGreenColorChange(Self);
end;

procedure TJvFullColorCircle.SetRedColor(const Value: TJvFullColor);
var
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

  InvalidateColors(OldColor, RedColor);

  if Assigned(FOnRedColorChange) then
    FOnRedColorChange(Self);
end;

procedure TJvFullColorCircle.SetCrossCenter(Value: Integer);
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

procedure TJvFullColorCircle.SetCrossSize(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FCrossSize <> Value then
  begin
    FCrossSize := Value;
    CalcSize;
  end;
end;

procedure TJvFullColorCircle.SetCrossStyle(const Value: TPen);
begin
  FCrossStyle.Assign(Value);
  Invalidate;
end;

procedure TJvFullColorCircle.SetInvertRadius(const Value: Boolean);
begin
  if FInvertRadius <> Value then
  begin
    FInvertRadius := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvFullColorCircle.SetInvertRotation(const Value: Boolean);
begin
  if FInvertRotation <> Value then
  begin
    FInvertRotation := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvFullColorCircle.SetLineWidth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FLineWidth <> Value then
  begin
    FLineWidth := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvFullColorCircle.SetStyles(const Value: TJvFullColorCircleStyles);
begin
  if FStyles <> Value then
  begin
    FStyles := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvFullColorCircle.Notification(AComponent: TComponent;
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

procedure TJvFullColorCircle.SetBlueColorTrackBar(const Value: TJvFullColorTrackBar);
begin
  if (Value <> nil) and (Value <> FBlueColorTrackBar) and Value.Linked then
    raise EJvFullColorError.CreateResFmt(@RsEDuplicateTrackBar, [Value.LinkerName]);

  if Assigned(FBlueColorTrackBar) then
  begin
    FBlueColorTrackBar.OnColorChange := nil;
    FBlueColorTrackBar.OnAxisConfigChange := nil;
    FBlueColorTrackBar.FreeLink;
  end;

  ReplaceComponentReference(Self, Value, TComponent(FBlueColorTrackBar));

  if Assigned(FBlueColorTrackBar) then
  begin
    FBlueColorTrackBar.OnColorChange := TrackBarColorChange;
    FBlueColorTrackBar.OnAxisConfigChange := TrackBarAxisConfigChange;
    FBlueColorTrackBar.FullColor := BlueColor;
    FBlueColorTrackBar.AxisConfig := AxisConfig;
    FBlueColorTrackBar.SetLink(Self);
  end;
end;

procedure TJvFullColorCircle.SetGreenColorTrackBar(const Value: TJvFullColorTrackBar);
begin
  if (Value <> nil) and (Value <> FGreenColorTrackBar) and Value.Linked then
    raise EJvFullColorError.CreateResFmt(@RsEDuplicateTrackBar, [Value.LinkerName]);

  if Assigned(FGreenColorTrackBar) then
  begin
    FGreenColorTrackBar.OnColorChange := nil;
    FGreenColorTrackBar.OnAxisConfigChange := nil;
    FGreenColorTrackBar.FreeLink;
  end;

  ReplaceComponentReference(Self, Value, TComponent(FGreenColorTrackBar));

  if Assigned(FGreenColorTrackBar) then
  begin
    FGreenColorTrackBar.OnColorChange := TrackBarColorChange;
    FGreenColorTrackBar.OnAxisConfigChange := TrackBarAxisConfigChange;
    FGreenColorTrackBar.FullColor := GreenColor;
    FGreenColorTrackBar.AxisConfig := AxisConfig;
    FGreenColorTrackBar.SetLink(Self);
  end;
end;

procedure TJvFullColorCircle.SetRedColorTrackBar(const Value: TJvFullColorTrackBar);
begin
  if (Value <> nil) and (Value <> FRedColorTrackBar) and Value.Linked then
    raise EJvFullColorError.CreateResFmt(@RsEDuplicateTrackBar, [Value.LinkerName]);

  if Assigned(FRedColorTrackBar) then
  begin
    FRedColorTrackBar.OnColorChange := nil;
    FRedColorTrackBar.OnAxisConfigChange := nil;
    FRedColorTrackBar.FreeLink;
  end;

  ReplaceComponentReference(Self, Value, TComponent(FRedColorTrackBar));

  if Assigned(FRedColorTrackBar) then
  begin
    FRedColorTrackBar.OnColorChange := TrackBarColorChange;
    FRedColorTrackBar.OnAxisConfigChange := TrackBarAxisConfigChange;
    FRedColorTrackBar.FullColor := RedColor;
    FRedColorTrackBar.AxisConfig := AxisConfig;
    FRedColorTrackBar.SetLink(Self);
  end;
end;

procedure TJvFullColorCircle.SetCommonColorTrackBar(const Value: TJvFullColorTrackBar);
begin
  if (Value <> nil) and (Value <> FCommonColorTrackBar) and Value.Linked then
    raise EJvFullColorError.CreateResFmt(@RsEDuplicateTrackBar, [Value.LinkerName]);

  if Assigned(FCommonColorTrackBar) then
  begin
    FCommonColorTrackBar.OnColorChange := nil;
    FCommonColorTrackBar.OnAxisConfigChange := nil;
    FCommonColorTrackBar.FreeLink;
  end;

  ReplaceComponentReference(Self, Value, TComponent(FCommonColorTrackBar));

  if Assigned(FCommonColorTrackBar) then
  begin
    FCommonColorTrackBar.OnColorChange := TrackBarColorChange;
    FCommonColorTrackBar.OnAxisConfigChange := TrackBarAxisConfigChange;
    FCommonColorTrackBar.FullColor := FullColor;
    FCommonColorTrackBar.AxisConfig := AxisConfig;
    FCommonColorTrackBar.SetLink(Self);
  end;
end;

procedure TJvFullColorCircle.SetCrossBlueColor(const Value: TColor);
begin
  if FCrossBlueColor <> Value then
  begin
    FCrossBlueColor := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvFullColorCircle.SetCrossGreenColor(const Value: TColor);
begin
  if FCrossGreenColor <> Value then
  begin
    FCrossGreenColor := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvFullColorCircle.SetCrossRedColor(const Value: TColor);
begin
  if FCrossRedColor <> Value then
  begin
    FCrossRedColor := Value;
    WantDrawBuffer := True;
  end;
end;

procedure TJvFullColorCircle.AxisConfigChange;
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

procedure TJvFullColorCircle.TrackBarColorChange(Sender: TObject);
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

procedure TJvFullColorCircle.ColorSpaceChange;
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
  FArrowPosition := apNormal;
  FColorOrientation := coNormal;

  FArrowWidth := 9;
  FArrowColor := clBlack;
  FFullColorDrawing := True;
  FValueXAuto := True;
  FValueYAuto := True;
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
begin
  case Orientation of
    trHorizontal:
      begin
        FBuffer.Width := Max(Width - (2 * ArrowWidth),0);
        FBuffer.Height := Max(Height - ArrowWidth,0);
      end;
    trVertical:
      begin
        FBuffer.Width := Max(Width - ArrowWidth,0);
        FBuffer.Height := Max(Height - (2 * ArrowWidth),0);
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
  MinZ, MaxZ, ValueZ, IndexZ: Integer;
  TempColor: TJvFullColor;
  GraphicRange: Integer;
begin
  if (FCreating) or (Width = 0) or (Height = 0) or
    (FBuffer.Width = 0) or (FBuffer.Height = 0) then
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
    begin
      if Orientation = trHorizontal then
        GraphicRange := FBuffer.Width - 1
      else
        GraphicRange := FBuffer.Height - 1;
      for IndexZ := 0 to GraphicRange do
      begin
        if ColorOrientation = coInverse then
          ValueZ := MaxZ - ((IndexZ * (MaxZ - MinZ)) div GraphicRange)
        else
          ValueZ := ((IndexZ * (MaxZ - MinZ)) div GraphicRange) + MinZ;
        Pen.Color := ConvertToColor(SetAxisValue(TempColor, AxisZ, ValueZ));
        case Orientation of
          trHorizontal:
            begin
              MoveTo(IndexZ, 0);
              LineTo(IndexZ, Height - ArrowWidth);
            end;
          trVertical:
            begin
              MoveTo(0, IndexZ);
              LineTo(Width - ArrowWidth, IndexZ);
            end;
        end;
      end;
    end;
  end;
  inherited DrawBuffer;
end;

function TJvFullColorTrackBar.GetCursorPosition: TJvCursorPoints;
var
  AxisZ: TJvAxisIndex;
  PosZ, MaxAxis, MinAxis: Integer;
  GraphicRange: Integer;
begin
  AxisZ := GetIndexAxisZ(AxisConfig);

  with ColorSpace do
  begin
    MaxAxis := AxisMax[AxisZ];
    MinAxis := AxisMin[AxisZ];
    if Orientation = trHorizontal then
      GraphicRange := FBuffer.Width - 1
    else
      GraphicRange := FBuffer.Height - 1;
    PosZ := GetAxisValue(FullColor, AxisZ);
    if ColorOrientation = coInverse then
      PosZ := ((MaxAxis - PosZ) * GraphicRange) div (MaxAxis - MinAxis)
    else
      PosZ := ((PosZ - MinAxis) * GraphicRange) div (MaxAxis - MinAxis);
    Inc(PosZ, ArrowWidth);
  end;

  case Orientation of
    trHorizontal:
      begin
        Result[0].X := PosZ - ArrowWidth;
        Result[1].X := PosZ;
        Result[2].X := PosZ + ArrowWidth;
        case ArrowPosition of
          apNormal:
            begin
              Result[0].Y := 0;
              Result[1].Y := ArrowWidth;
              Result[2].Y := 0;
            end;
          apOpposite:
            begin
              Result[0].Y := Height - 1;
              Result[1].Y := Height - 1 - ArrowWidth;
              Result[2].Y := Height - 1;
            end;
        end;
      end;
    trVertical:
      begin
        Result[0].Y := PosZ - ArrowWidth;
        Result[1].Y := PosZ;
        Result[2].Y := PosZ + ArrowWidth;
        case ArrowPosition of
          apNormal:
            begin
              Result[0].X := 0;
              Result[1].X := ArrowWidth;
              Result[2].X := 0;
            end;
          apOpposite:
            begin
              Result[0].X := Width - 1;
              Result[1].X := Width - 1 - ArrowWidth;
              Result[2].X := Width - 1;
            end;
        end;
      end;
  end;
end;

procedure TJvFullColorTrackBar.InvalidateCursor;
var
  ARect: TRect;
  CursorPoints: TJvCursorPoints;
begin
  CursorPoints := GetCursorPosition;
  ARect.Left := Min(CursorPoints[0].X, Min(CursorPoints[1].X, CursorPoints[2].X));
  ARect.Top := Min(CursorPoints[0].Y, Min(CursorPoints[1].Y, CursorPoints[2].Y));
  ARect.Right := Max(CursorPoints[0].X, Max(CursorPoints[1].X, CursorPoints[2].X)) + 1;
  ARect.Bottom := Max(CursorPoints[0].Y, Max(CursorPoints[1].Y, CursorPoints[2].Y)) + 1;
  Windows.InvalidateRect(Handle, @ARect, False);
end;

procedure TJvFullColorTrackBar.Paint;
var
  CursorPoints: TJvCursorPoints;
begin
  inherited Paint;

  with Canvas do
  begin
    case Orientation of
      trHorizontal:
        case ArrowPosition of
          apNormal:
            begin
              DrawFrame(ArrowWidth, ArrowWidth + 1);
              Draw(ArrowWidth, ArrowWidth + 1, FBuffer);
            end;
          apOpposite:
            begin
              DrawFrame(ArrowWidth, 0);
              Draw(ArrowWidth, 0, FBuffer);
            end;
        end;
      trVertical:
        case ArrowPosition of
          apNormal:
            begin
              DrawFrame(ArrowWidth + 1, ArrowWidth);
              Draw(ArrowWidth + 1, ArrowWidth, FBuffer);
            end;
          apOpposite:
            begin
              DrawFrame(0, ArrowWidth);
              Draw(0, ArrowWidth, FBuffer);
            end;
        end;
    end;
    Brush.Color := ArrowColor;
    Pen.Color := ArrowColor;
    CursorPoints := GetCursorPosition;
    Polygon(CursorPoints);
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

procedure TJvFullColorTrackBar.KeyMove(KeyCode: TJvKeyCode; MoveCount: Integer);
var
  IndexAxisZ: TJvAxisIndex;
  ValueZ: Integer;
begin
  IndexAxisZ := GetIndexAxisZ(AxisConfig);
  ValueZ := GetAxisValue(FullColor, IndexAxisZ);

  if ColorOrientation = coInverse then
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
  GraphicRange: Integer;
  Pos: Integer;
begin
  if (not (ssLeft in Shift)) or (FBuffer.Width = 0) or (FBuffer.Height = 0) or
   (Width = 0) or (Height = 0) then
    Exit;
  if Orientation = trHorizontal then
    Pos := X - ArrowWidth
  else
    Pos := Y - ArrowWidth;

  if Orientation = trHorizontal then
    GraphicRange := FBuffer.Width - 1
  else
    GraphicRange := FBuffer.Height - 1;

  AxisZ := GetIndexAxisZ(AxisConfig);
  with ColorSpace do
  begin
    MinZ := AxisMin[AxisZ];
    MaxZ := AxisMax[AxisZ];

    Pos := EnsureRange((Pos * (MaxZ - MinZ)) div GraphicRange, 0, MaxZ - MinZ);
    if ColorOrientation = coInverse then
      Pos := MaxZ - Pos
    else
      Pos := Pos + MinZ;

    FullColor := SetAxisValue(FullColor, AxisZ, Pos);
  end;
  inherited MouseColor(Shift, X, Y);
end;

procedure TJvFullColorTrackBar.SetArrowColor(const Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    Invalidate;
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
  if FArrowWidth <> Value then
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

procedure TJvFullColorTrackBar.SetColorOrientation(const Value: TJvFullColorOrientation);
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
  OldValueX, OldValueY, OldValueZ, NewValueZ: Byte;
begin
  if Value <> FullColor then
  begin
    AxisZ := GetIndexAxisZ(AxisConfig);
    OldValueZ := GetAxisValue(FullColor, AxisZ);
    NewValueZ := GetAxisValue(Value, AxisZ);
    if NewValueZ <> OldValueZ then
      InvalidateCursor;
    if FullColorDrawing then
    begin
      OldValueX := ValueX;
      OldValueY := ValueY;
      inherited SetFullColor(Value);
      if ValueXAuto then
        UpdateDefaultValueX;
      if ValueYAuto then
        UpdateDefaultValueY;
      if (ValueX <> OldValueX) or (ValueY <> OldValueY) then
        WantDrawBuffer := True;
    end
    else
      inherited SetFullColor(Value);
    if NewValueZ <> OldValueZ then
      InvalidateCursor;
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
  if FullColorDrawing then
    FValueX := GetAxisValue(FullColor, GetIndexAxisX(AxisConfig))
  else
    FValueX := ColorSpace.AxisDefault[GetIndexAxisX(AxisConfig)];
end;

procedure TJvFullColorTrackBar.UpdateDefaultValueY;
begin
  if FullColorDrawing then
    FValueY := GetAxisValue(FullColor, GetIndexAxisY(AxisConfig))
  else
    FValueY := ColorSpace.AxisDefault[GetIndexAxisY(AxisConfig)];
end;

//=== { TJvColorLabel } ======================================================

constructor TJvFullColorLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque];
  FPen := TPen.Create;
  FPen.OnChange := GraphicChange;
  FBrush := TBrush.Create;
  FBrush.OnChange := GraphicChange;
  FShapeType := stRectangle;
  FShapePosition := spLeft;
  FSpacing := 5;
  FRoundShapeWidth := 4;
  FRoundShapeHeight := 4;
  FShapeWidth := 16;
  FShapeHeight := 16;
  FLabelColor := fclDEFWindowText;
  Width := 100;
  Height := 25;
end;

destructor TJvFullColorLabel.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TJvFullColorLabel.CalcSize;
begin
  Canvas.Font := Font;
  if AutoSize then
  begin
    case ShapePosition of
      spLeft..spRight:
        begin
          Height := Max(ShapeHeight + Pen.Width, Canvas.TextHeight(Caption));
          Width := ShapeWidth + Pen.Width + Spacing + Canvas.TextWidth(FCaption);
        end;
      spTop..spBottom:
        begin
          Height := ShapeHeight + Spacing + Pen.Width + Canvas.TextHeight(Caption);
          Width := Max(ShapeWidth + Pen.Width, Canvas.TextWidth(Caption));
        end;
    end;
    AdjustSize;
  end
  else
    Invalidate;
end;

procedure TJvFullColorLabel.GraphicChange(Sender: TObject);
begin
  CalcSize;
  Invalidate;
end;

procedure TJvFullColorLabel.Paint;
var
  ShapeLeft, ShapeTop, TextLeft, TextTop: Integer;
begin
  with Canvas do
  begin
    Pen.Style := psClear;
    Brush.Style := bsSolid;
    Brush.Color := Parent.Brush.Color;
    Rectangle(0, 0, Width, Height);

    Font := Self.Font;
    Pen := Self.Pen;
    Brush := Self.Brush;
    Brush.Color := ColorSpaceManager.ConvertToColor(LabelColor);
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
    else
      ShapeLeft := 0;
      ShapeTop := 0;
      TextLeft := 0;
      TextTop := 0;
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

procedure TJvFullColorLabel.SetAutoSize(Value: Boolean);
begin
  inherited SetAutoSize(Value);
  CalcSize;
end;

procedure TJvFullColorLabel.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    CalcSize;
  end;
end;

procedure TJvFullColorLabel.SetName(const Value: TComponentName);
var
  Equal: Boolean;
begin
  Equal := Name = FCaption;
  inherited SetName(Value);
  if Equal then
    Caption := Name;
end;

procedure TJvFullColorLabel.SetRoundShapeHeight(const Value: Integer);
begin
  if (Value <> FRoundShapeHeight) and (Value < ShapeHeight div 2) then
  begin
    FRoundShapeHeight := Value;
    Invalidate;
  end;
end;

procedure TJvFullColorLabel.SetRoundShapeWidth(const Value: Integer);
begin
  if (Value <> FRoundShapeWidth) and (Value < ShapeWidth div 2) then
  begin
    FRoundShapeWidth := Value;
    if FRoundShapeWidth > Value div 2 then
      FRoundShapeWidth := Value div 2;
    if Shape in [stSquare, stRoundSquare, stCircle] then
      FShapeHeight := FShapeWidth;
    Invalidate;
  end;
end;

procedure TJvFullColorLabel.SetShapeHeight(const Value: Integer);
begin
  if FShapeHeight <> Value then
  begin
    FShapeHeight := Value;
    if FRoundShapeHeight > Value div 2 then
      FRoundShapeHeight := Value div 2;
    if Shape in [stSquare, stRoundSquare, stCircle] then
      FShapeWidth := FShapeHeight;
    CalcSize;
  end;
end;

procedure TJvFullColorLabel.SetShapePosition(const Value: TJvShapePosition);
begin
  if FShapePosition <> Value then
  begin
    FShapePosition := Value;
    CalcSize;
  end;
end;

procedure TJvFullColorLabel.SetShapeType(const Value: TShapeType);
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

procedure TJvFullColorLabel.SetShapeWidth(const Value: Integer);
begin
  if FShapeWidth <> Value then
  begin
    FShapeWidth := Value;
    if Shape in [stSquare, stRoundSquare, stCircle] then
      FShapeHeight := FShapeWidth;
    CalcSize;
  end;
end;

procedure TJvFullColorLabel.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    CalcSize;
  end;
end;

procedure TJvFullColorLabel.SetLabelColor(const Value: TJvFullColor);
begin
  if FLabelColor <> Value then
  begin
    FLabelColor := Value;
    Brush.Color := ColorSpaceManager.ConvertToColor(Value);
    Invalidate;
  end;
end;

procedure TJvFullColorLabel.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
  Invalidate;
end;

procedure TJvFullColorLabel.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
  CalcSize;
end;

//=== { TJvColorSpaceCombo } =================================================

constructor TJvFullColorSpaceCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csDropDownList;
  FAllowVariable := True;
  FItemFormat := cfBoth;
end;

procedure TJvFullColorSpaceCombo.CreateWnd;
begin
  inherited CreateWnd;
  MakeList;
end;

function TJvFullColorSpaceCombo.GetColorSpace: TJvColorSpace;
begin
  if ItemIndex > -1 then
    Result := TJvColorSpace(Self.Items.Objects[ItemIndex])
  else
    Result := nil;
end;

function TJvFullColorSpaceCombo.GetColorSpaceID: TJvFullColorSpaceID;
var
  CS: TJvColorSpace;
begin
  CS := SelectedSpace;
  if CS <> nil then
    Result := CS.ID
  else
    Result := csRGB;
end;

procedure TJvFullColorSpaceCombo.MakeList;
var
  Index: Integer;
  LColorSpace: TJvColorSpace;
  OldColorID: TJvFullColorSpaceID;
  ACaption: string;
begin
  OldColorID := ColorSpaceID;
  with ColorSpaceManager, Items do
  begin
    Clear;
    for Index := 0 to ColorSpaceManager.Count - 1 do
    begin
      LColorSpace := ColorSpaceByIndex[Index];
      if (LColorSpace.ID <> csDEF) or AllowVariable then
      begin
        if Assigned(FOnFormatItem) then
          FOnFormatItem(Self, LColorSpace, ACaption)
        else
          ACaption := ColorSpaceToString(LColorSpace, ItemFormat);
        AddObject(ACaption, LColorSpace);
      end;
    end;
  end;
  ColorSpaceID := OldColorID;
end;

procedure TJvFullColorSpaceCombo.SetAllowVariable(const Value: Boolean);
begin
  if FAllowVariable <> Value then
  begin
    FAllowVariable := Value;
    MakeList;
  end;
end;

procedure TJvFullColorSpaceCombo.SetColorSpace(const Value: TJvColorSpace);
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    if Value.ID = TJvColorSpace(Items.Objects[I]).ID then
    begin
      ItemIndex := I;
      Exit;
    end;
end;

procedure TJvFullColorSpaceCombo.SetColorSpaceID(const Value: TJvFullColorSpaceID);
begin
  SetColorSpace(ColorSpaceManager.ColorSpace[Value]);
end;

procedure TJvFullColorSpaceCombo.SetItemFormat(const Value: TJvFullColorSpaceFormat);
begin
  if FItemFormat <> Value then
  begin
    FItemFormat := Value;
    MakeList;
  end;
end;

//=== { TJvColorAxisConfigCombo } ============================================

constructor TJvFullColorAxisCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csDropDownList;
  FColorID := csRGB;
  FItemFormat := afComplete;
end;

procedure TJvFullColorAxisCombo.CreateWnd;
begin
  inherited CreateWnd;
  MakeList;
end;

function TJvFullColorAxisCombo.GetSelected: TJvFullColorAxisConfig;
begin
  if ItemIndex = -1 then
    Result := acXYZ
  else
    Result := TJvFullColorAxisConfig(ItemIndex);
end;

procedure TJvFullColorAxisCombo.MakeList;
var
  Index: TJvFullColorAxisConfig;
  LColorSpace: TJvColorSpace;
  OldItemIndex: Integer;
  FormattedItem: string;
begin
  OldItemIndex := ItemIndex;
  LColorSpace := ColorSpaceManager.ColorSpace[ColorID];
  with Items do
  begin
    Clear;
    for Index := Low(TJvFullColorAxisConfig) to High(TJvFullColorAxisConfig) do
    begin
      if Assigned(FOnFormatItem) then
        FOnFormatItem(Self,Index,FormattedItem)
      else
        FormattedItem := AxisConfigToString(Index, ItemFormat, LColorSpace);
      Add(FormattedItem);
    end;
  end;
  if OldItemIndex > -1 then
    ItemIndex := OldItemIndex
  else
    ItemIndex := 0;
end;

procedure TJvFullColorAxisCombo.SetColorID(const Value: TJvFullColorSpaceID);
begin
  if FColorID <> Value then
  begin
    FColorID := Value;
    MakeList;
  end;
end;

procedure TJvFullColorAxisCombo.SetItemFormat(const Value: TJvFullColorAxisConfigFormat);
begin
  if FItemFormat <> Value then
  begin
    FItemFormat := Value;
    MakeList;
  end;
end;

procedure TJvFullColorAxisCombo.SetOnFormatItem(
  const Value: TJvFullColorAxisFormatEvent);
begin
  FOnFormatItem := Value;
  MakeList;
end;

procedure TJvFullColorAxisCombo.SetSelected(const Value: TJvFullColorAxisConfig);
begin
  ItemIndex := Ord(Value);
end;

//=== { TJvFullColorList } ===================================================

constructor TJvFullColorList.Create;
begin
  inherited Create;
  FList := nil;
  FCount := 0;
  FCapacity := 0;
  FAllocBy := 2;
end;

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
  Change(Result, foAdded);
end;

procedure TJvFullColorList.Assign(Source: TPersistent);
var
  Index: Integer;
begin
  if Source is TJvFullColorList then
    with TJvFullColorList(Source) do
    begin
      Self.BeginUpdate;
      Self.Count := Count;
      for Index := 0 to Self.Count - 1 do
        Self.Items[Index] := Items[Index];
      Self.EndUpdate;
      Self.Change(-1, foAllChanged);
    end
  else
    inherited Assign(Source);
end;

procedure TJvFullColorList.Change(AIndex: Integer;
  AOperation: TJvFullColorListOperation);
begin
  if (UpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self, AIndex, AOperation);
end;

procedure TJvFullColorList.Clear;
begin
  Capacity := 0;
  Change(-1, foAllChanged);
end;

procedure TJvFullColorList.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Items', ReadItems, WriteItems, Count > 0);
end;

procedure TJvFullColorList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    EJvFullColorListError.CreateFmt(SListIndexError, [Index]);

  Dec(FCount);
  if Index < Count then
    Move(FList^[Index + 1], FList^[Index], (Count - Index) * SizeOf(TJvFullColor));

  Change(Index, foDeleted);
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

  Change(Index1, foChanged);
  Change(Index2, foChanged);
end;

function TJvFullColorList.GetItem(Index: Integer): TJvFullColor;
begin
  if (Index >= Count) or (Index < 0) then
    raise EJvFullColorListError.CreateResFmt(@SListIndexError, [Index]);

  Result := FList^[Index];
end;

procedure TJvFullColorList.Grow;
begin
  Capacity := Capacity + AllocBy;
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
  if (Index > Count) or (Index < 0) then
    EJvFullColorListError.CreateFmt(SListIndexError, [Index]);

  if Count = Capacity then
    Grow;

  if Index < Count then
    Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(TJvFullColor));

  FList^[Index] := AColor;
  Inc(FCount);

  Change(Index, foAdded);
end;

procedure TJvFullColorList.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TJvFullColorList.ReadItems(Reader: TReader);
begin
  try
    Reader.ReadListBegin;
    BeginUpdate;
    Clear;
    while not Reader.EndOfList do
      Add(Reader.ReadInteger);
  finally
    EndUpdate;
    Reader.ReadListEnd;
  end;
end;

function TJvFullColorList.Remove(AColor: TJvFullColor): Integer;
begin
  Result := IndexOf(AColor);
  if Result >= 0 then
    Delete(Result);
end;

procedure TJvFullColorList.SetAllocBy(const Value: Integer);
begin
  FAllocBy := Max(Value, 1);
end;

procedure TJvFullColorList.SetCapacity(const Value: Integer);
begin
  ReallocMem(FList, Value * SizeOf(TJvFullColor));
  FCapacity := Value;
  if FCount > FCapacity then
  begin
    FCount := FCapacity;
    Change(-1, foAllChanged);
  end;
end;

procedure TJvFullColorList.SetCount(const Value: Integer);
begin
  FCount := Value;
  if FCount > FCapacity then
    Capacity := FCount;
  Change(-1, foAllChanged);
end;

procedure TJvFullColorList.SetItem(Index: Integer; const Value: TJvFullColor);
begin
  if (Index >= Count) or (Index < 0) then
    EJvFullColorListError.CreateFmt(SListIndexError, [Index]);

  FList^[Index] := Value;
  Change(Index, foChanged);
end;

procedure TJvFullColorList.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Change(-1, foAllChanged);
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

//=== { TFullColorGroup } ====================================================

constructor TJvFullColorGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FItems := TJvFullColorList.Create;
  FItems.OnChange := ItemsChange;
  FBrush := TBrush.Create;
  FBrush.OnChange := BrushChange;
  FEdge := feRaised;
  FSelectedEdge := feLowered;
  FMouseEdge := feRaised;
  FColCount := 5;
  FSquareSize := 6;
  FSelectedIndex := -1;
  FMouseIndex := -1;
  BevelKind := bkTile;
  Width := 100;
  Height := 100;
end;

destructor TJvFullColorGroup.Destroy;
begin
  FItems.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TJvFullColorGroup.CalcRects(out XPos, YPos, XInc, YInc: Integer);
var
  XOffset: Integer;
  YOffset: Integer;
begin
  XOffset := Width - (FSquareSize * ColCount) - 2;
  XInc := XOffset div ColCount;
  XPos := ((XOffset - (XInc * (ColCount - 1))) div 2) + 1;

  YOffset := Height - (FSquareSize * RowCount) - 2;
  YInc := YOffset div RowCount;
  YPos := ((YOffset - (YInc * (RowCount - 1))) div 2) + 1;
end;

procedure TJvFullColorGroup.ItemsChange(Sender: TObject; Index: Integer;
  Operation: TJvFullColorListOperation);
begin
  case Operation of
    foAllChanged:
      begin
        FMouseIndex := -1;
        FSelectedIndex := -1;
        Invalidate;
      end;
    foDeleted:
      begin
        FMouseIndex := -1;
        FSelectedIndex := EnsureRange(FSelectedIndex, -1, Items.Count - 1);
        Invalidate;
      end;
    foAdded:
      Invalidate;
    foChanged:
      InvalidateIndex(Index);
  end;
end;

procedure TJvFullColorGroup.BrushChange(Sender: TObject);
begin
  Refresh;
end;

procedure TJvFullColorGroup.InvalidateIndex(AIndex: Integer);
var
  ARect: TRect;
  ColIndex, RowIndex: Integer;
  XPos, YPos, XInc, YInc: Integer;
begin
  if AIndex <> -1 then
  begin
    CalcRects(XPos, YPos, XInc, YInc);
    ColIndex := AIndex mod ColCount;
    RowIndex := AIndex div ColCount;
    ARect.Left := XPos + ColIndex * (XInc + FSquareSize);
    ARect.Top := YPos + RowIndex * (YInc + FSquareSize);
    ARect.Right := ARect.Left + FSquareSize + 1;
    ARect.Bottom := ARect.Top + FSquareSize + 1;
    Windows.InvalidateRect(Handle, @ARect, False);
  end;
end;

procedure TJvFullColorGroup.MouseLeave(var Msg: TWMMouse);
begin
  FMouseIndex := -1;
  Msg.Result := 1;
  Refresh;
end;

{$IFDEF RTL200_UP}
// for D2009 "Use Controls.PHintInfo" warning
type
  PHintInfo = Controls.PHintInfo;
{$ENDIF RTL200_UP}

procedure TJvFullColorGroup.CMHintShow(var Msg: TMessage);
var
  AHintInfo: PHintInfo;
  Sum, XPos, YPos, XInc, YInc, Index: Integer;
  ColorIndex: Integer;
  AFullColor: TJvFullColor;
  AColor: TColor;
  AColorID: TJvFullColorSpaceID;
  AColorSpace: TJvColorSpace;
begin
  AHintInfo := PHintInfo(Msg.LParam);
  ColorIndex := -1;

  CalcRects(XPos, YPos, XInc, YInc);

  Sum := YPos;
  with AHintInfo^ do
    for Index := 0 to RowCount - 1 do
    begin
      if CursorPos.Y < Sum then
      begin
        CursorRect.Top := Max(0, Sum - YInc);
        CursorRect.Bottom := Sum;
        Break;
      end
      else
      if (CursorPos.Y >= Sum) and (CursorPos.Y < Sum + FSquareSize) then
      begin
        CursorRect.Top := Sum;
        CursorRect.Bottom := Sum + FSquareSize;
        ColorIndex := Index * ColCount;
        Break;
      end;
      Inc(Sum, FSquareSize + YInc);
    end;

  Sum := XPos;
  with AHintInfo^ do
    for Index := 0 to ColCount do
      // not -1 because of last space after the colcount - 1
    begin
      if CursorPos.X < Sum then
      begin
        CursorRect.Left := Max(0, Sum - XInc);
        CursorRect.Right := Sum;
        ColorIndex := -1;
        Break;
      end
      else
      if (CursorPos.X >= Sum) and (CursorPos.X < Sum + FSquareSize) then
      begin
        CursorRect.Left := Sum;
        CursorRect.Right := Sum + FSquareSize;
        if ColorIndex <> -1 then
          ColorIndex := ColorIndex + Index;
        Break;
      end;
      Inc(Sum, FSquareSize + XInc);
    end;

  if ColorIndex >= Items.Count then
    ColorIndex := -1;

  if ColorIndex > -1 then
    with ColorSpaceManager do
    begin
      AFullColor := Items.Items[ColorIndex];
      AColorID := GetColorSpaceID(AFullColor);
      AColorSpace := ColorSpace[AColorID];

      if AColorSpace.ID = csDEF then
        with TJvDEFColorSpace(AColorSpace) do
        begin
          AColor := ConvertToColor(AFullColor);
          for Index := 0 to ColorCount - 1 do
            if AColor = ColorValue[Index] then
            begin
              AHintInfo.HintStr := Format(RsColorHintFmt1, [AFullColor,
                AColorSpace.Name, AColorID, ColorName[Index], ColorPrettyName[Index]]);
              Break;
            end;
        end
      else
        AHintInfo.HintStr := Format(RsColorHintFmt2, [AFullColor, AColorSpace.Name, AColorID,
          AColorSpace.AxisName[axIndex0], GetAxisValue(AFullColor, axIndex0),
          AColorSpace.AxisName[axIndex1], GetAxisValue(AFullColor, axIndex1),
          AColorSpace.AxisName[axIndex2], GetAxisValue(AFullColor, axIndex2)]);

      if Assigned(FOnFormatHint) then
        FOnFormatHint(Self, AFullColor, AHintInfo.HintStr);
    end
  else
    AHintInfo.HintStr := Hint;

  Msg.Result := 0;
end;

procedure TJvFullColorGroup.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  Sum: Integer;
  XPos, YPos, XInc, YInc: Integer;
  ColIndex, RowIndex: Integer;
begin
  inherited MouseMove(Shift, X, Y);

  CalcRects(XPos, YPos, XInc, YInc);

  Sum := XPos;
  if X < XPos then
  begin
    InvalidateIndex(MouseIndex);
    FMouseIndex := -1;
    Exit;
  end;
  ColIndex := -1;
  for Index := 0 to ColCount - 1 do
  begin
    if (X >= Sum) and (X < Sum + FSquareSize) then
    begin
      ColIndex := Index;
      Break;
    end;
    if (X >= Sum + FSquareSize) and (X < Sum + FSquareSize + XInc) then
      Break;
    Inc(Sum, FSquareSize + XInc);
  end;

  if ColIndex = -1 then
  begin
    InvalidateIndex(MouseIndex);
    FMouseIndex := -1;
    Exit;
  end;

  Sum := YPos;
  if Y < YPos then
  begin
    InvalidateIndex(MouseIndex);
    FMouseIndex := -1;
    Exit;
  end;
  RowIndex := -1;
  for Index := 0 to RowCount - 1 do
  begin
    if (Y >= Sum) and (Y < Sum + FSquareSize) then
    begin
      RowIndex := Index;
      Break;
    end;
    if (Y >= Sum + FSquareSize) and (Y < Sum + FSquareSize + YInc) then
      Break;
    Inc(Sum, FSquareSize + YInc);
  end;
  if RowIndex = -1 then
  begin
    InvalidateIndex(MouseIndex);
    FMouseIndex := -1;
    Exit;
  end;

  InvalidateIndex(MouseIndex);
  FMouseIndex := RowIndex * ColCount + ColIndex;
  if MouseIndex > Items.Count - 1 then
    FMouseIndex := -1;
  InvalidateIndex(MouseIndex);
end;

procedure TJvFullColorGroup.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  InvalidateIndex(SelectedIndex);
  SelectedIndex := MouseIndex;
  InvalidateIndex(SelectedIndex);
end;

procedure TJvFullColorGroup.Paint;
var
  Index, IndexX, IndexY, XMaj: Integer;
  XOffset, YOffset, XInc, YInc: Integer;
  X, Y: Integer;
  Edge: TJvFullColorEdge;
  ClipRect: TRect;

  procedure BevelRect(const R: TRect; Style: TJvFullColorEdge;
    FillStyle: TBrushStyle; FillColor: TColor);
  var
    Color1, Color2: TColor;
  begin
    case Style of
      feLowered:
        begin
          Color1 := clBtnShadow;
          Color2 := clBtnHighlight;
        end;
      feRaised:
        begin
          Color1 := clBtnHighlight;
          Color2 := clBtnShadow;
        end;
    else
      Color1 := clBlack;
      Color2 := clBlack;
    end;

    with Canvas do
    begin
      Brush.Color := FillColor;
      Brush.Style := FillStyle;
      Pen.Color := FillColor;
      Pen.Style := psClear;
      Rectangle(R.Left + 1, R.Top + 1, R.Right + 1, R.Bottom + 1);

      Pen.Style := psSolid;
      Pen.Color := Color1;
      PolyLine([Point(R.Left, R.Bottom), Point(R.Left, R.Top),
        Point(R.Right, R.Top)]);
      Pen.Color := Color2;
      PolyLine([Point(R.Right, R.Top), Point(R.Right, R.Bottom),
        Point(R.Left, R.Bottom)]);
    end;
  end;

begin
  inherited Paint;
  CalcRects(XOffset, YOffset, XInc, YInc);

  Y := YOffset;
  X := XOffset;
  ClipRect := Canvas.ClipRect;

  Index := 0;
  while Index < Items.Count do
  begin
    if Index = SelectedIndex then
      Edge := SelectedEdge
    else
    if Index = MouseIndex then
      Edge := MouseEdge
    else
      Edge := feFlat;

    BevelRect(Rect(X, Y, X + FSquareSize, Y + FSquareSize), Edge, Brush.Style,
      ColorSpaceManager.ConvertToColor(Items[Index]));
    Inc(Index);
    if Index mod ColCount = 0 then
    begin
      X := XOffset;
      Inc(Y, YInc + FSquareSize);
    end
    else
      Inc(X, XInc + FSquareSize);
  end;

  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    Pen.Color := Color;
    Y := YOffset;
    for IndexY := 0 to RowCount do
    begin
      Rectangle(Max(ClipRect.Left, 1), Max(Y - YInc + 1, 1),
        Min(ClipRect.Right, Width - 2), Min(Y, Height - 2));
      X := XOffset;
      for IndexX := 0 to ColCount do
      begin
        if IndexX + IndexY * ColCount >= Items.Count then
          XMaj := FSquareSize + 1
        else
          XMaj := 0;
        Rectangle(Max(X - XInc + 1, 1), Min(Max(Y, 1), Height - 2),
          Min(X + XMaj, Width - 2), Min(Y + FSquareSize + 1, Height - 2));
        Inc(X, XInc + FSquareSize);
      end;
      Inc(Y, YInc + FSquareSize);
    end;
  end;
end;

procedure TJvFullColorGroup.SetEdge(const Value: TJvFullColorEdge);
begin
  FEdge := Value;
  Refresh;
end;

procedure TJvFullColorGroup.SetMouseEdge(const Value: TJvFullColorEdge);
begin
  FMouseEdge := Value;
  Refresh;
end;

procedure TJvFullColorGroup.SetSelectedEdge(const Value: TJvFullColorEdge);
begin
  FSelectedEdge := Value;
  Refresh;
end;

procedure TJvFullColorGroup.SetColCount(const Value: Integer);
begin
  if Value <= 0 then
    FColCount := 1
  else
    FColCount := Value;
  Refresh;
end;

function TJvFullColorGroup.GetRowCount: Integer;
begin
  Result := Max((Items.Count + ColCount - 1) div ColCount, 1);
end;

procedure TJvFullColorGroup.SetItems(const Value: TJvFullColorList);
begin
  FItems.Assign(Value);
  Invalidate;
end;

procedure TJvFullColorGroup.SetSquareSize(const Value: Integer);
var
  TempValue: Integer;
begin
  if FSquareSize < 0 then
    FSquareSize := -FSquareSize;

  if FSquareSize = 0 then
    FSquareSize := 1;

  FSquareSize := Value;

  TempValue := (Width - 2) div ColCount;
  if TempValue < FSquareSize then
    FSquareSize := TempValue;

  TempValue := (Height - 2) div (Items.Count div ColCount + 1);
  if TempValue < FSquareSize then
    FSquareSize := TempValue;

  Refresh;
end;

function TJvFullColorGroup.GetSelected: TJvFullColor;
begin
  if SelectedIndex > -1 then
    Result := Items[SelectedIndex]
  else
    Result := clNone;
end;

procedure TJvFullColorGroup.SetSelected(const Value: TJvFullColor);
begin
  SelectedIndex := Items.IndexOf(Value);
end;

procedure TJvFullColorGroup.SetSelectedIndex(const Value: Integer);
begin
  FSelectedIndex := EnsureRange(Value, -1, Items.Count - 1);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvFullColorGroup.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
