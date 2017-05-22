{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBCtrl.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Polaris Software

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:

    === NEW IN JVCL 3.0 ==
        TJvDBMaskEdit is a new control, added by Warren Postma.

    Major Issues:
        EditMask property enables operation as masked edit, which doesn't
        work properly in a Control Grid, yet, if you set the EditMask.
        You can use it as a generic editor control inside a control grid.
          -- Warren Postma (warrenpstma att hotmail dott com)
-----------------------------------------------------------------------------}
// $Id$

unit JvDBControls;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows,
  Messages,
  Classes, Graphics, Controls, StdCtrls, DBCtrls, DB,
  JvSecretPanel, JvLabel, JvToolEdit, JvMaskEdit, JvBaseEdits;

type
  { NEW VALIDATION EVENT }
  TJvDBAcceptValueEvent = procedure(Sender: TObject; OldValue: string;
    var NewValue: string; var Accept, Post: Boolean) of object;

  {NEW IN JVCL3.0 - Enhanced DBEdit/DBMaskEdit }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDBMaskEdit = class(TJvCustomMaskEdit) // same base as TJvMaskEdit, plus data aware.
  private
    {Standard data-aware crap}
    FDataLink: TFieldDataLink;
    FCanvas: TControlCanvas;
    FFocused: Boolean;

    {new: Specific to this component}
    // value of text in the edit control at the time
    // that keyboard focus enters the control:
    FOriginalValue: string;
    // Validation/event.
    FOnAcceptNewValue: TJvDBAcceptValueEvent;
    FDefaultMask: Boolean;
    procedure ActiveChange(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetCanvas: TCanvas;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure ResetMaxLength;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
    function GetReadOnly: Boolean; reintroduce;
    procedure SetReadOnly(Value: Boolean); reintroduce;
    function GetEditMask: string;
    procedure SetEditMask(const AValue: string);
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure WMCut(var Msg: TMessage); message WM_CUT;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure WMUndo(var Msg: TMessage); message WM_UNDO;
    procedure Change; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Reset; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property Canvas: TCanvas read GetCanvas;
  published
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property ClickKey;
    property ClipboardCommands;
    property Color;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DisabledColor;
    property DisabledTextColor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Flat;
    property ParentFlat;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    {Common JEDI Niceties}
    property BeepOnError;
    { designtime properties SPECIFIC to only JvDBMaskEdit: }
    property EditMask: string read GetEditMask write SetEditMask;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    {new event}
    // This event is fired when a new value has been entered, and the Enter key is
    // hit, and the mask checking worked, and we are asking the user
    // for whether to accept the entry, or not, and if so, the end
    // user may also want to automatically set a flag to cause an automatic Post
    // after the db control does a write to the fieldlink.
    property OnAcceptNewValue: TJvDBAcceptValueEvent read FOnAcceptNewValue write FOnAcceptNewValue;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDBComboEdit = class(TJvCustomComboEdit)
  private
    FDataLink: TFieldDataLink;
    FCanvas: TControlCanvas;
    FFocused: Boolean;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetCanvas: TCanvas;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    function GetReadOnly: Boolean; reintroduce;
    procedure SetReadOnly(Value: Boolean); reintroduce;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure WMCut(var Msg: TMessage); message WM_CUT;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure Change; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Reset; override;
    property AlwaysEnableButton default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Button;
    property Field: TField read GetField;
    property Canvas: TCanvas read GetCanvas;
  published
    property AlwaysShowPopup default False;
    property Alignment;
    property Align;
    property Action;
    property AutoSelect;
    property AutoSize;
    property BeepOnError;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BorderStyle;
    property ButtonFlat;
    property ButtonHint;
    property CharCase;
    property ClickKey;
    property Color;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Flat;
    property ParentFlat;
    property Font;
    property Glyph;
    property ImageIndex;
    property Images;
    property ImageKind;
    property ButtonWidth;
    property HideSelection;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property NumGlyphs;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnButtonClick;
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
    property OnEndDock;
    property OnStartDock;
    (* ++ RDB ++ *)
    property ClipboardCommands;
    property DisabledTextColor;
    property DisabledColor;
    (* -- RDB -- *)
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDBDateEdit = class(TJvCustomDateEdit)
  private
    FInReset: Boolean; // Polaris
    FDataLink: TFieldDataLink;
    FCanvas: TControlCanvas;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetCanvas: TCanvas;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure UpdateData(Sender: TObject);
    procedure AfterPopup(Sender: TObject; var Date: TDateTime; var Action: Boolean);
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    function GetReadOnly: Boolean; reintroduce;
    procedure SetReadOnly(Value: Boolean); reintroduce;
  protected
    procedure DoExit; override;
    procedure WMCut(var Msg: TMessage); message WM_CUT;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure AcceptValue(const Value: Variant); override;
    procedure ApplyDate(Value: TDateTime); override;
    procedure Change; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Reset; override;
    function DisplayNullDateAsEmptyText: Boolean; override;
    // Polaris
    procedure SetDate(Value: TDateTime); override;
    function IsValidDate(Value: TDateTime): Boolean;
    // Polaris
    procedure PopupDropDown(DisableEdit: Boolean); override;
    property AlwaysEnableButton default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateMask; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property Canvas: TCanvas read GetCanvas;
  published
    property AlwaysShowPopup default False;
    property DateAutoBetween;
    property MinDate;
    property MaxDate;
    property Align;
    property Alignment;
    property Action;
    property AutoSize;
    property BeepOnError;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property AutoSelect;
    property BlanksChar;
    property BorderStyle;
    property ButtonHint;
    property ButtonFlat;
    property CalendarHints;
    property CheckOnExit;
    property ClickKey;
    property Color;
    property DefaultToday;
    property DialogTitle;
    property DirectInput;
    property DragCursor;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property DragKind;
    property Flat;
    property ParentBiDiMode;
    property ParentFlat;
    property ImeMode;
    property ImeName;
    property OnEndDock;
    property OnStartDock;
    property DragMode;
    property Enabled;
    property Font;
    property Glyph;
    property GroupIndex;
    property ImageIndex;
    property Images;
    property ImageKind;
    property NumGlyphs;
    property ButtonWidth;
    property HideSelection;
    property Anchors;
    property Constraints;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupAlign;
    property PopupColor;
    property PopupMenu;
    property ShowHint;
    property CalendarStyle;
    property ShowNullDate;
    property StartOfWeek;
    property Weekends;
    property WeekendColor;
    property YearDigits;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
    property ClipboardCommands; // RDB
    property DisabledTextColor; // RDB
    property DisabledColor; // RDB
    property OnKeyDown; // RDB
    property OnPopupHidden;
    property OnPopupShown;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDBCalcEdit = class(TJvCalcEdit)
  private
    FDataLink: TFieldDataLink;
    FDefaultParams: Boolean;
    //Polaris
    FLEmptyIsNull: Boolean;
    FEmptyIsNull: Boolean;
    procedure SetEmptyIsNull(Value: Boolean);
    function GetZeroEmpty: Boolean;
    procedure SetZeroEmpty(Value: Boolean);
    function StoreEmptyIsNull: Boolean;
    //Polaris
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetDefaultParams(Value: Boolean);
    procedure UpdateFieldData(Sender: TObject);
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
    function GetReadOnly: Boolean; reintroduce;
    procedure SetReadOnly(Value: Boolean); reintroduce;
  protected
    procedure DoExit; override;
    procedure WMCut(var Msg: TMessage); message WM_CUT;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure AcceptValue(const Value: Variant); override;
    function GetDisplayText: string; override;
    procedure Change; override;
    procedure SetText(const AValue: string); override;

    procedure DataChanged; override; //Polaris

    function EditCanModify: Boolean; override;
    function IsValidChar(Key: Char): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Reset; override;
    procedure UpdatePopup; override;
    //Polaris
    procedure Loaded; override;
    //Polaris
    procedure PopupDropDown(DisableEdit: Boolean); override;
    property AlwaysEnableButton default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateFieldParams;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property Value;
  published
    property AlwaysShowPopup default False;
    property Align;
    property DecimalPlaceRound;

    property Action;
    property AutoSize;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DefaultParams: Boolean read FDefaultParams write SetDefaultParams default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Alignment;
    property AutoSelect;
    property BeepOnError;
    property BorderStyle;
    property ButtonHint;
    property ButtonFlat;
    property CheckOnExit;
    property ClickKey;
    property Color;
    property DecimalPlaces;
    property DirectInput;
    property DisplayFormat;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FormatOnEditing;
    property ImageIndex;
    property Images;
    property ImageKind;
    property ButtonWidth;
    property HideSelection;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupAlign;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    //Polaris
    property EmptyIsNull: Boolean read FEmptyIsNull write SetEmptyIsNull stored StoreEmptyIsNull;
    property ZeroEmpty: Boolean read GetZeroEmpty write SetZeroEmpty default True;
    //Polaris
    property OnButtonClick;
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
    property OnEndDock;
    property OnStartDock;
    property OnPopupHidden; // RH: Added - issue 5726
    property OnPopupShown; // RH: Added - issue 5726
    (* ++ RDB ++ *)
    property ClipboardCommands;
    property DisabledTextColor;
    property DisabledColor;
    (* -- RDB -- *)
  end;

  TGetStringEvent = function(Sender: TObject): string of object;
  TDataValueEvent = procedure(Sender: TObject; DataSet: TDataSet; var Value: Longint) of object;
  TDBLabelStyle = (lsState, lsRecordNo, lsRecordSize);
  TGlyphAlign = glGlyphLeft..glGlyphRight;
  TDBStatusKind = dsInactive..dsCalcFields;
  TDBLabelOptions = (doCaption, doGlyph, doBoth);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDBStatusLabel = class(TJvCustomLabel)
  private
    FDataSetName: string;
    FStyle: TDBLabelStyle;
    FEditColor: TColor;
    FCalcCount: Boolean;
    FCaptions: TStringList;
    FGlyph: TBitmap;
    FCell: TBitmap;
    FGlyphAlign: TGlyphAlign;
    FOnGetDataName: TGetStringEvent;
    FOnGetRecNo: TDataValueEvent;
    FOnGetRecordCount: TDataValueEvent;
    function GetStatusKind(State: TDataSetState): TDBStatusKind;
    procedure CaptionsChanged(Sender: TObject);
    function GetDataSetName: string;
    procedure SetDataSetName(Value: string);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function GetDatasetState: TDataSetState;
    procedure SetEditColor(Value: TColor);
    procedure SetStyle(Value: TDBLabelStyle);
    procedure SetShowOptions(Value: TDBLabelOptions);
    procedure SetGlyphAlign(Value: TGlyphAlign);
    function GetCaptions: TStrings;
    procedure SetCaptions(Value: TStrings);
    procedure SetCalcCount(Value: Boolean);
  protected
    FDataLink: TDataLink;
    FRecordCount: Longint;
    FRecordNo: Longint;
    FShowOptions: TDBLabelOptions;
    procedure Loaded; override;
    function GetDefaultFontColor: TColor; override;
    function GetLabelCaption: string; override;
    function GetCaption(State: TDataSetState): string; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateData; virtual;
    procedure UpdateStatus; virtual;
    property Caption;
    property DatasetState: TDataSetState read GetDatasetState;
  published
    property DataSetName: string read GetDataSetName write SetDataSetName;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property EditColor: TColor read FEditColor write SetEditColor default clRed;
    property Captions: TStrings read GetCaptions write SetCaptions;
    property Style: TDBLabelStyle read FStyle write SetStyle default lsState;
    property CalcRecCount: Boolean read FCalcCount write SetCalcCount default False;
    property ShowOptions: TDBLabelOptions read FShowOptions write SetShowOptions default doCaption;
    property GlyphAlign: TGlyphAlign read FGlyphAlign write SetGlyphAlign default glGlyphLeft;
    property Layout default tlCenter;
    property ShadowSize default 0;
    property Align;
    property Alignment;
    property AutoSize;
    property Color;
    property DragCursor;
    property DragMode;
    property Font;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShadowColor;
    property ShadowPos;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnGetDataName: TGetStringEvent read FOnGetDataName write FOnGetDataName;
    property OnGetRecordCount: TDataValueEvent read FOnGetRecordCount write FOnGetRecordCount;
    property OnGetRecNo: TDataValueEvent read FOnGetRecNo write FOnGetRecNo;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDBNavigator = class(TDBNavigator)
  private
    FTransparent: Boolean;
    procedure SetTransparent(Value: Boolean);
  protected
    procedure Paint; override;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Transparent: Boolean read FTransparent write SetTransparent default True;
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
  FMTBcd,
  Variants, SysUtils, Math, Forms,
  JvJCLUtils, JvJVCLUtils, JvCalc, JvTypes, JvConsts, JvResources, JclSysUtils;

{$R JvDBControls.res}

function IsNullOrEmptyStringField(Field: TField): Boolean;
begin
  Result := Field.IsNull or ((Field is TStringField) and (Trim(Field.AsString) = ''));
end;

//=== { TJvDBMaskEdit } ======================================================

constructor TJvDBMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnActiveChange := ActiveChange;
  // new stuff that isn't in the VCL version.
  inherited ReadOnly := True;
end;

destructor TJvDBMaskEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

procedure TJvDBMaskEdit.Loaded;
begin
  inherited Loaded;
  FDefaultMask := (inherited EditMask = '');
  ResetMaxLength;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

procedure TJvDBMaskEdit.ResetMaxLength;
var
  F: TField;
begin
  if (MaxLength > 0) and Assigned(DataSource) and Assigned(DataSource.DataSet) then
  begin
    F := DataSource.DataSet.FindField(DataField);
    if Assigned(F) and (F.DataType in [ftString, ftWideString]) and (F.Size = MaxLength) then
      MaxLength := 0;
  end;
end;

procedure TJvDBMaskEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then
    DataSource := nil;
end;

function TJvDBMaskEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

procedure TJvDBMaskEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if ((Key = VK_DELETE) and (Shift * KeyboardShiftStates = [])) or
    ((Key = VK_INSERT) and (Shift * KeyboardShiftStates = [ssShift])) then
    FDataLink.Edit;
end;

procedure TJvDBMaskEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if CharInSet(Key, [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    DoBeepOnError;
    Key := #0;
  end;
  case Key of
    CtrlH, CtrlV, CtrlX, #32..#255:
      FDataLink.Edit;
    Esc:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

function TJvDBMaskEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

procedure TJvDBMaskEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TJvDBMaskEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (Alignment <> taLeftJustify) and not IsMasked then
      Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TJvDBMaskEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

function TJvDBMaskEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBMaskEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
  begin
    if FDataLink.DataSource <> nil then
      FDataLink.DataSource.RemoveFreeNotification(Self);
    FDataLink.DataSource := Value;
  end;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

function TJvDBMaskEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBMaskEdit.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength;
  FDataLink.FieldName := Value;
end;

function TJvDBMaskEdit.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TJvDBMaskEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJvDBMaskEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TJvDBMaskEdit.GetEditMask: string;
begin
  if FDefaultMask then
    Result := ''
  else
    Result := inherited EditMask;
end;

procedure TJvDBMaskEdit.SetEditMask(const AValue: string);
begin
  inherited EditMask := AValue;
  FDefaultMask := False;
end;

function TJvDBMaskEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJvDBMaskEdit.ActiveChange(Sender: TObject);
begin
  ResetMaxLength;
end;

procedure TJvDBMaskEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if Alignment <> FDataLink.Field.Alignment then
    begin
      EditText := '';  {forces update}
      Alignment := FDataLink.Field.Alignment;
    end;
    if EditMask = '' then
    begin
      inherited EditMask := FDataLink.Field.EditMask;
      FDefaultMask := True;
    end;
    if not (csDesigning in ComponentState) then
      if (FDataLink.Field.DataType in [ftString, ftWideString]) and (MaxLength = 0) then
        MaxLength := FDataLink.Field.Size;
    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
    begin
      EditText := FDataLink.Field.DisplayText;
      if FDataLink.Editing {and FDataLink.FModified XXX } then
        Modified := True;
    end;
  end
  else
  begin
    Alignment := taLeftJustify;
    //EditMask := '';
    if csDesigning in ComponentState then
      EditText := Name
    else
      EditText := '';
  end;
end;

procedure TJvDBMaskEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TJvDBMaskEdit.UpdateData(Sender: TObject);
var
  OrgDefaultMask: Boolean;
  OrgMask: string;
begin
  ValidateEdit;
  if IsMasked then
  begin
    OrgDefaultMask := FDefaultMask;
    OrgMask := inherited EditMask;
    try
      EditMask := '';
      if Text = '' then
      begin
        FDataLink.Field.Clear;
        Exit;
      end;
    finally
      FDefaultMask := OrgDefaultMask;
      EditMask := OrgMask;
    end;
  end;
  FDataLink.Field.Text := Text;
end;

procedure TJvDBMaskEdit.WMUndo(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBMaskEdit.WMPaste(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBMaskEdit.WMCut(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBMaskEdit.DoEnter;
begin
  FOriginalValue := Self.Text;
  SetFocused(True);
  inherited DoEnter;
  if SysLocale.FarEast and FDataLink.CanModify then
    inherited ReadOnly := False;
end;

procedure TJvDBMaskEdit.DoExit;
var
  NewValue: string;
  Accept, Post: Boolean;
begin
  Accept := True;
  Post := False;
  NewValue := Text;
  // When we hit enter, check if there was a change, and if so,
  // we can fire the confirmation event.
  if FOriginalValue <> NewValue then
    if Assigned(FOnAcceptNewValue) then
    begin
      FOnAcceptNewValue(Self, FOriginalValue, NewValue, Accept, Post);
      if not Accept then
        Text := FOriginalValue;
    end;
  try
   if Accept then
     FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;
  if Accept then
    inherited DoExit;

  { A nifty little way to keep simple database applications happy.
    Just set POST flag in your validation, and the dataset is updated.
    If you don't like this feature, just DON'T set Post to true, it
    defaults to false.
  }
  if (Accept and Post) and (Assigned(DataSource)) then
    if Assigned(DataSource.DataSet) and (DataSource.DataSet.Active) then
      if DataSource.DataSet.State = dsEdit then
        DataSource.DataSet.Post;
end;

procedure TJvDBMaskEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := LRESULT(FDataLink);
end;

function TJvDBMaskEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TJvDBMaskEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

//=== { TJvDBComboEdit } =====================================================

procedure ResetMaxLength(DBEdit: TJvDBComboEdit);
var
  F: TField;
begin
  with DBEdit do
    if (MaxLength > 0) and (DataSource <> nil) and
      (DataSource.DataSet <> nil) then
    begin
      F := DataSource.DataSet.FindField(DataField);
      if Assigned(F) and (F.DataType = ftString) and
        (F.Size = MaxLength) then
        MaxLength := 0;
    end;
end;

constructor TJvDBComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  inherited ReadOnly := True;
  AlwaysEnableButton := True;
  AlwaysShowPopup := False;
end;

destructor TJvDBComboEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

procedure TJvDBComboEdit.Loaded;
begin
  inherited Loaded;
  ResetMaxLength(Self);
  if csDesigning in ComponentState then
    DataChange(Self);
end;

procedure TJvDBComboEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TJvDBComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if ((Key = VK_DELETE) and (Shift * KeyboardShiftStates = [])) or
    ((Key = VK_INSERT) and (Shift * KeyboardShiftStates = [ssShift])) then
    FDataLink.Edit;
end;

procedure TJvDBComboEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if CharInSet(Key, [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    DoBeepOnError;
    Key := #0;
  end;
  case Key of
    CtrlH, CtrlV, CtrlX, #32..#255:
      FDataLink.Edit;
    Esc:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

function TJvDBComboEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

procedure TJvDBComboEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TJvDBComboEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (Alignment <> taLeftJustify) and not IsMasked then
      Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TJvDBComboEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

function TJvDBComboEdit.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TJvDBComboEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBComboEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
  begin
    if FDataLink.DataSource <> nil then
      FDataLink.DataSource.RemoveFreeNotification(Self);
    FDataLink.DataSource := Value;
  end;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

function TJvDBComboEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBComboEdit.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength(Self);
  FDataLink.FieldName := Value;
end;

function TJvDBComboEdit.GetReadOnly: Boolean;
begin
  if FDataLink <> nil then
    Result := FDataLink.ReadOnly
  else
    Result := True;
end;

procedure TJvDBComboEdit.SetReadOnly(Value: Boolean);
begin
  if FDataLink <> nil then
    FDataLink.ReadOnly := Value;
end;

function TJvDBComboEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJvDBComboEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if Alignment <> FDataLink.Field.Alignment then
    begin
      EditText := ''; {forces update}
      Alignment := FDataLink.Field.Alignment;
    end;
    EditMask := FDataLink.Field.EditMask;
    if not (csDesigning in ComponentState) then
      if (FDataLink.Field.DataType = ftString) and (MaxLength = 0) then
        MaxLength := FDataLink.Field.Size;
    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
    begin
      EditText := FDataLink.Field.DisplayText;
      {if FDataLink.Editing then Modified := True;}
    end;
  end
  else
  begin
    Alignment := taLeftJustify;
    EditMask := '';
    if csDesigning in ComponentState then
      EditText := Name
    else
      EditText := '';
  end;
end;

procedure TJvDBComboEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TJvDBComboEdit.UpdateData(Sender: TObject);
begin
  ValidateEdit;
  FDataLink.Field.Text := Text;
end;

procedure TJvDBComboEdit.WMPaste(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBComboEdit.WMCut(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBComboEdit.DoEnter;
begin
  SetFocused(True);
  inherited DoEnter;
  if SysLocale.FarEast and FDataLink.CanModify then
    inherited ReadOnly := False;
end;

procedure TJvDBComboEdit.DoExit;
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then
      SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;
  inherited DoExit;
end;

procedure TJvDBComboEdit.WMPaint(var Msg: TWMPaint);
var
  S: string;
begin
  if csDestroying in ComponentState then
    Exit;
  if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
  begin
    S := FDataLink.Field.DisplayText;
    case CharCase of
      ecUpperCase:
        S := AnsiUpperCase(S);
      ecLowerCase:
        S := AnsiLowerCase(S);
    end;
  end
  else
    S := EditText;
  if not PaintComboEdit(Self, S, Alignment, True, FCanvas, Msg) then
    inherited;
end;

procedure TJvDBComboEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := LRESULT(FDataLink);
end;

function TJvDBComboEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TJvDBComboEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TJvDBComboEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

//=== { TJvDBDateEdit } ======================================================

constructor TJvDBDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FInReset := False; // Polaris
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  Self.OnAcceptDate := AfterPopup;
  AlwaysEnableButton := True;
  AlwaysShowPopup := False;
  inherited ReadOnly := True;
  UpdateMask;
end;

destructor TJvDBDateEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

function TJvDBDateEdit.DisplayNullDateAsEmptyText: Boolean;
begin
  Result := inherited DisplayNullDateAsEmptyText;
  
  if FDataLink.Field <> nil then
    if FDataLink.Field.IsNull then
      Result := True;
end;

procedure TJvDBDateEdit.AfterPopup(Sender: TObject; var Date: TDateTime;
  var Action: Boolean);
begin
  Action := Action and (DataSource <> nil) and (DataSource.DataSet <> nil) and
    DataSource.DataSet.CanModify;
  if Action then
    Action := EditCanModify;
end;

procedure TJvDBDateEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TJvDBDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if not ReadOnly and
    ((Key = VK_DELETE) and (Shift * KeyboardShiftStates = [])) or
    ((Key = VK_INSERT) and (Shift * KeyboardShiftStates = [ssShift])) then
    FDataLink.Edit;
end;

procedure TJvDBDateEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if CharInSet(Key, [#32..#255]) and (FDataLink.Field <> nil) and
    not CharInSet(Key, DigitChars) and (Key <> JclFormatSettings.DateSeparator) then
  begin
    DoBeepOnError;
    Key := #0;
  end;
  case Key of
    CtrlH, CtrlV, CtrlX, '0'..'9':
      FDataLink.Edit;
    Esc:
      begin
        Reset;
        Key := #0;
      end;
  end;
end;

function TJvDBDateEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

procedure TJvDBDateEdit.Reset;
begin
  FInReset := True; // Polaris
  try
    FDataLink.Reset;
    SelectAll;
  finally
    FInReset := False; // Polaris
  end;
end;

// Polaris begin

function TJvDBDateEdit.IsValidDate(Value: TDateTime): Boolean;
begin
  Result := FDateAutoBetween;
  if not Result then
    if not FInReset and FDataLink.Editing then
    try
      if Value <> NullDate then
      begin
        if (MinDate <> NullDate) and (MaxDate <> NullDate) and
          ((Value < MinDate) or (Value > MaxDate)) then
          raise EJVCLException.CreateResFmt(@RsEDateOutOfRange, [FormatDateTime(GetDateFormat, Value),
            FormatDateTime(GetDateFormat, MinDate), FormatDateTime(GetDateFormat, MaxDate)])
        else
        if (MinDate <> NullDate) and (Value < MinDate) then
          raise EJVCLException.CreateResFmt(@RsEDateOutOfMin, [FormatDateTime(GetDateFormat, Value),
            FormatDateTime(GetDateFormat, MinDate)])
        else
        if (MaxDate <> NullDate) and (Value > MaxDate) then
          raise EJVCLException.CreateResFmt(@RsEDateOutOfMax, [FormatDateTime(GetDateFormat, Value),
            FormatDateTime(GetDateFormat, MaxDate)]);
      end;
      Result := True;
    except
      Reset;
      raise;
    end;
end;

procedure TJvDBDateEdit.SetDate(Value: TDateTime);
begin
  IsValidDate(Value);
  inherited SetDate(Value);
end;

// Polaris end

procedure TJvDBDateEdit.Change;
begin
  if not Formatting then
    FDataLink.Modified;
  inherited Change;
end;

procedure TJvDBDateEdit.PopupDropDown(DisableEdit: Boolean);
begin
  {if not ReadOnly then} // checked in FDataLink.Edit via CanModify
  if AlwaysShowPopup or FDataLink.Edit then
    inherited PopupDropDown(DisableEdit);
end;

function TJvDBDateEdit.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TJvDBDateEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBDateEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
  begin
    if FDataLink.DataSource <> nil then
      FDataLink.DataSource.RemoveFreeNotification(Self);
    FDataLink.DataSource := Value;
  end;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

function TJvDBDateEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBDateEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TJvDBDateEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJvDBDateEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TJvDBDateEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJvDBDateEdit.UpdateMask;
begin
  UpdateFormat;
  UpdatePopup;
  DataChange(nil);
end;

procedure TJvDBDateEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    EditMask := GetDateMask;
    if IsNullOrEmptyStringField(FDataLink.Field) then
      inherited SetDate(NullDate)
    else
      inherited SetDate(FDataLink.Field.AsDateTime);
  end
  else
  begin
    if csDesigning in ComponentState then
    begin
      EditMask := '';
      EditText := Name;
    end
    else
    begin
      EditMask := GetDateMask;
      if DefaultToday then
        Date := SysUtils.Date
      else
        Date := NullDate;
    end;
  end;
end;

procedure TJvDBDateEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
  if FDataLink.Editing and DefaultToday and (FDataLink.Field <> nil) and
    (IsNullOrEmptyStringField(FDataLink.Field) or (FDataLink.Field.AsDateTime = NullDate)) then
    FDataLink.Field.AsDateTime := SysUtils.Now;
end;

procedure TJvDBDateEdit.UpdateData(Sender: TObject);
var
  D: TDateTime;
begin
  ValidateEdit;
  D := Self.Date;
  if D <> NullDate then
  begin
    if Int(FDataLink.Field.AsDateTime) <> D then
      FDataLink.Field.AsDateTime := D + Frac(FDataLink.Field.AsDateTime);
  end
  else
    FDataLink.Field.Clear;
end;

procedure TJvDBDateEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := LRESULT(FDataLink);
end;

procedure TJvDBDateEdit.WMPaint(var Msg: TWMPaint);
var
  S: string;
begin
  if csDestroying in ComponentState then
    Exit;
  if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
  begin
    if FDataLink.Field.IsNull then
    begin
      S := GetDateFormat;
      S := ReplaceStr(ReplaceStr(ReplaceStr(ReplaceStr(S, '/', JclFormatSettings.DateSeparator),
        'Y', ' '), 'M', ' '), 'D', ' ');
    end
    else
      S := FormatDateTime(GetDateFormat, FDataLink.Field.AsDateTime);
  end
  else
    S := EditText;
  if not PaintComboEdit(Self, S, Alignment, True, FCanvas, Msg) then
    inherited;
end;

procedure TJvDBDateEdit.AcceptValue(const Value: Variant);
begin
  if VarIsNullEmpty(Value) then
    FDataLink.Field.Clear
  else
    FDataLink.Field.AsDateTime :=
      VarToDateTime(Value) + Frac(FDataLink.Field.AsDateTime);
  DoChange;
end;

procedure TJvDBDateEdit.ApplyDate(Value: TDateTime);
begin
  FDataLink.Edit;
  inherited ApplyDate(Value);
end;

procedure TJvDBDateEdit.WMPaste(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBDateEdit.WMCut(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBDateEdit.DoExit;
begin
  try
    if not (csDesigning in ComponentState) and CheckOnExit then
      CheckValidDate;
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then
      SetFocus;
    raise;
  end;
  CheckCursor;
  inherited DoExit;
end;

function TJvDBDateEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TJvDBDateEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TJvDBDateEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

//Polaris

//=== { TJvDBCalcEdit } ======================================================

constructor TJvDBCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  //Polaris
  FEmptyIsNull := ZeroEmpty;
  FLEmptyIsNull := True;
  //Polaris
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateFieldData;
  inherited ReadOnly := True;
  AlwaysShowPopup := False;
  AlwaysEnableButton := True;
end;

destructor TJvDBCalcEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TJvDBCalcEdit.Loaded;
begin
  inherited Loaded;
  FLEmptyIsNull := True;
end;

procedure TJvDBCalcEdit.SetEmptyIsNull(Value: Boolean);
begin
  if Value <> FEmptyIsNull then
  begin
    FEmptyIsNull := Value;
    if csLoading in ComponentState then
      FLEmptyIsNull := False;
  end;
end;

function TJvDBCalcEdit.GetZeroEmpty: Boolean;
begin
  Result := inherited ZeroEmpty;
end;

procedure TJvDBCalcEdit.SetZeroEmpty(Value: Boolean);
begin
  inherited ZeroEmpty := Value;
  if FLEmptyIsNull then
    SetEmptyIsNull(ZeroEmpty)
end;

function TJvDBCalcEdit.StoreEmptyIsNull: Boolean;
begin
  Result := FEmptyIsNull <> ZeroEmpty;
end;

//Polaris

procedure TJvDBCalcEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TJvDBCalcEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if not ReadOnly and
    ((Key = VK_DELETE) and (Shift * KeyboardShiftStates = [])) or
    ((Key = VK_INSERT) and (Shift * KeyboardShiftStates = [ssShift])) then
    FDataLink.Edit;
end;

procedure TJvDBCalcEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    CtrlH, CtrlV, CtrlX, #32..#255:
      if not PopupVisible then
        FDataLink.Edit;
    Esc:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

function TJvDBCalcEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := inherited IsValidChar(Key);
  if Result and (FDataLink.Field <> nil) then
    Result := FDataLink.Field.IsValidChar(Key);
end;

procedure TJvDBCalcEdit.UpdatePopup;
var
  Precision: Byte;
begin
  Precision := DefCalcPrecision;
  if (FDataLink <> nil) and (FDataLink.Field <> nil) and
    (FDataLink.Field is TFloatField) then
    Precision := TFloatField(FDataLink.Field).Precision;
  if FPopup <> nil then
    SetupPopupCalculator(FPopup, Precision, BeepOnError);
end;

procedure TJvDBCalcEdit.PopupDropDown(DisableEdit: Boolean);
begin
  {if not ReadOnly then} // checked in FDataLink.Edit via CanModify
  if AlwaysShowPopup or FDataLink.Edit then
    inherited PopupDropDown(DisableEdit);
end;

function TJvDBCalcEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

function TJvDBCalcEdit.GetDisplayText: string;
var
  E: Extended;
begin
  if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
  begin
    if FDataLink.Field.IsNull then
      E := 0.0
    else
    if FDataLink.Field.DataType in [ftSmallint, ftInteger, ftWord] then
      E := FDataLink.Field.AsInteger
    else
    if FDataLink.Field.DataType = ftBoolean then
      E := Ord(FDataLink.Field.AsBoolean)
    else
    if FDataLink.Field is TLargeintField then
      E := TLargeintField(FDataLink.Field).AsLargeInt
    else
      E := FDataLink.Field.AsFloat;
    if FDataLink.Field.IsNull then
      Result := ''
    else
      Result := FormatDisplayText(E);
  end
  else
  begin
    if FDataLink.Field = nil then
    begin
      if csDesigning in ComponentState then
        Result := Format('(%s)', [Name])
      else
        Result := '';
    end
    else
    //Polaris Result := inherited GetDisplayText;
    if FDataLink.Field.IsNull then
      Result := ''
    else
      Result := inherited GetDisplayText;
    //Polaris
  end;
end;

procedure TJvDBCalcEdit.Reset;
begin
  FDataLink.Reset;
  inherited Reset;
end;

procedure TJvDBCalcEdit.Change;
begin
  if not Formatting then
    FDataLink.Modified;
  inherited Change;
end;

procedure TJvDBCalcEdit.SetText(const AValue: string);
begin
  if not ReadOnly then
    inherited SetText(AValue);
end;

//Polaris
procedure TJvDBCalcEdit.DataChanged;
begin
  inherited;
  if Assigned(FDataLink) and Assigned(FDataLink.Field) {and DecimalPlaceRound} then
  begin
    EditText := DisplayText;
    try
      if EditText <> '' then
        if (StrToFloat(TextToValText(EditText)) = 0) and ZeroEmpty then
          EditText := '';
    except
    end;
  end;
end;
//Polaris

function TJvDBCalcEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBCalcEdit.SetDataSource(Value: TDataSource);
begin
  if FDataLink.DataSource <> Value then
  begin
    if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    begin
      if FDataLink.DataSource <> nil then
        FDataLink.DataSource.RemoveFreeNotification(Self);
      FDataLink.DataSource := Value;
    end;
    if Value <> nil then
      Value.FreeNotification(Self);
    UpdateFieldParams;
  end;
end;

function TJvDBCalcEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBCalcEdit.SetDataField(const Value: string);
begin
  if FDataLink.FieldName <> Value then
  begin
    FDataLink.FieldName := Value;
    UpdateFieldParams;
  end;
end;

procedure TJvDBCalcEdit.SetDefaultParams(Value: Boolean);
begin
  if DefaultParams <> Value then
  begin
    FDefaultParams := Value;
    if FDefaultParams then
      UpdateFieldParams;
  end;
end;

procedure TJvDBCalcEdit.UpdateFieldParams;
begin
  if FDataLink.Field <> nil then
  begin
    if FDataLink.Field is TNumericField then
    begin
      if TNumericField(FDataLink.Field).DisplayFormat <> '' then
        DisplayFormat := TNumericField(FDataLink.Field).DisplayFormat;
      Alignment := TNumericField(FDataLink.Field).Alignment;
    end;
    if FDataLink.Field is TLargeintField then
    begin
      MaxValue := TLargeintField(FDataLink.Field).MaxValue;
      MinValue := TLargeintField(FDataLink.Field).MinValue;
      DecimalPlaces := 0;
      if DisplayFormat = '' then
        DisplayFormat := ',#';
    end
    else
    if FDataLink.Field is TIntegerField then
    begin
      MaxValue := TIntegerField(FDataLink.Field).MaxValue;
      MinValue := TIntegerField(FDataLink.Field).MinValue;
      DecimalPlaces := 0;
      if DisplayFormat = '' then
        DisplayFormat := ',#';
    end
    else
    if FDataLink.Field is TBCDField then
    begin
      MaxValue := TBCDField(FDataLink.Field).MaxValue;
      MinValue := TBCDField(FDataLink.Field).MinValue;
    end
    else
    if FDataLink.Field is TFloatField then
    begin
      MaxValue := TFloatField(FDataLink.Field).MaxValue;
      MinValue := TFloatField(FDataLink.Field).MinValue;
        //Polaris      DecimalPlaces := TFloatField(FDataLink.Field).Precision;
      DecimalPlaces := Min(DecimalPlaces, TFloatField(FDataLink.Field).Precision);
    end
    else
    if FDataLink.Field is TBooleanField then
    begin
      MinValue := 0;
      MaxValue := 1;
      DecimalPlaces := 0;
      if DisplayFormat = '' then
        DisplayFormat := ',#';
    end;
  end;
  UpdatePopup;
end;

function TJvDBCalcEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJvDBCalcEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TJvDBCalcEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJvDBCalcEdit.DataChange(Sender: TObject);
begin
  if FDefaultParams then
    UpdateFieldParams;
  if FDataLink.Field <> nil then
  begin
    if FDataLink.Field.IsNull then
    begin
      Self.Value := 0.0;
      EditText := '';
    end
    else
    if FDataLink.Field.DataType in [ftSmallint, ftInteger, ftWord] then
      Self.AsInteger := FDataLink.Field.AsInteger
    else
    if FDataLink.Field.DataType = ftBoolean then
      Self.AsInteger := Ord(FDataLink.Field.AsBoolean)
    else
    if FDataLink.Field is TLargeintField then
      Self.Value := TLargeintField(FDataLink.Field).AsLargeInt
    else
      Self.Value := FDataLink.Field.AsFloat;
    DataChanged;
  end
  else
  begin
    if csDesigning in ComponentState then
    begin
      Self.Value := 0;
      EditText := Format('(%s)', [Name]);
    end
    else
      Self.Value := 0;
  end;
end;

procedure TJvDBCalcEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TJvDBCalcEdit.UpdateFieldData(Sender: TObject);
begin
  inherited UpdateData;
  //Polaris  if (Value = 0) and ZeroEmpty then FDataLink.Field.Clear
  if (Trim(Text) = '') and FEmptyIsNull then
    FDataLink.Field.Clear
      //if (Value = 0) and ZeroEmpty then
//  FDataLink.Field.Clear
  else

  case FDataLink.Field.DataType of
    ftSmallint,
    ftInteger,
    ftWord:
      begin
        FDataLink.Field.AsInteger := Self.AsInteger;
      end;
    ftBoolean:
      begin
        FDataLink.Field.AsBoolean := Boolean(Self.AsInteger);
      end;
    ftFMTBcd,
    ftBCD:
      begin
        FDataLink.Field.AsBCD := DoubleToBCD(Self.Value)
      end;
    else
      begin
        FDataLink.Field.AsFloat := Self.Value;
      end;
  end;
end;

procedure TJvDBCalcEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := LRESULT(FDataLink);
end;

procedure TJvDBCalcEdit.AcceptValue(const Value: Variant);
begin
  if VarIsNullEmpty(Value) then
    FDataLink.Field.Clear
  else
    FDataLink.Field.Value := CheckValue(Value, False);
  DoChange;
end;

procedure TJvDBCalcEdit.WMPaste(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TJvDBCalcEdit.WMCut(var Msg: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

// Polaris
procedure TJvDBCalcEdit.DoExit;
begin
  if Modified then
  try
    CheckRange;
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then
      SetFocus;
    raise;
  end;
  inherited DoExit;
end;

function TJvDBCalcEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TJvDBCalcEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TJvDBCalcEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

//=== { TJvStatusDataLink } ==================================================

type
  TJvStatusDataLink = class(TDataLink)
  private
    FLabel: TJvDBStatusLabel;
  protected
    procedure ActiveChanged; override;
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure LayoutChanged; override;
  public
    constructor Create(ALabel: TJvDBStatusLabel);
    destructor Destroy; override;
  end;

constructor TJvStatusDataLink.Create(ALabel: TJvDBStatusLabel);
begin
  inherited Create;
  FLabel := ALabel;
end;

destructor TJvStatusDataLink.Destroy;
begin
  FLabel := nil;
  inherited Destroy;
end;

procedure TJvStatusDataLink.ActiveChanged;
begin
  DataSetChanged;
end;

procedure TJvStatusDataLink.DataSetScrolled(Distance: Integer);
begin
  if (FLabel <> nil) and (FLabel.Style = lsRecordNo) then
    FLabel.UpdateStatus;
end;

procedure TJvStatusDataLink.EditingChanged;
begin
  if (FLabel <> nil) and (FLabel.Style <> lsRecordSize) then
    FLabel.UpdateStatus;
end;

procedure TJvStatusDataLink.DataSetChanged;
begin
  if FLabel <> nil then
    FLabel.UpdateData;
end;

procedure TJvStatusDataLink.LayoutChanged;
begin
  if (FLabel <> nil) and (FLabel.Style <> lsRecordSize) then
    DataSetChanged; { ??? }
end;

//=== { TJvDBStatusLabel } ===================================================

const
  GlyphSpacing = 2;
  GlyphColumns = 7;

constructor TJvDBStatusLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ShadowSize := 0;
  Layout := tlCenter;
  ControlStyle := ControlStyle - [csSetCaption , csReplicatable];
  FRecordCount := -1;
  FRecordNo := -1;
  ShowAccelChar := False;
  FDataSetName := '';
  FDataLink := TJvStatusDataLink.Create(Self);
  FStyle := lsState;
  GlyphAlign := glGlyphLeft;
  FEditColor := clRed;
  FCaptions := TStringList.Create;
  FCaptions.OnChange := CaptionsChanged;
  FGlyph := TBitmap.Create;
  FGlyph.Handle := LoadBitmap(HInstance, 'JvDBStatusLabelSTATES');
  Caption := '';
end;

destructor TJvDBStatusLabel.Destroy;
begin
  FreeAndNil(FDataLink);
  //DisposeStr(FDataSetName);
  FCaptions.OnChange := nil;
  FreeAndNil(FCaptions);
  FreeAndNil(FCell);
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

procedure TJvDBStatusLabel.Loaded;
begin
  inherited Loaded;
  UpdateData;
end;

function TJvDBStatusLabel.GetDefaultFontColor: TColor;
begin
  if (FStyle = lsState) and (FDataLink <> nil) and
    (GetDatasetState in [dsEdit, dsInsert]) then
    Result := FEditColor
  else
    Result := inherited GetDefaultFontColor;
end;

function TJvDBStatusLabel.GetLabelCaption: string;
begin
  if (csDesigning in ComponentState) and ((FStyle = lsState) or
    (FDataLink = nil) or not FDataLink.Active) then
    Result := Format('(%s)', [Name])
  else
  if (FDataLink = nil) or (DataSource = nil) then
    Result := ''
  else
  begin
    case FStyle of
      lsState:
        if FShowOptions in [doCaption, doBoth] then
        begin
          if DataSetName = '' then
            Result := GetCaption(DataSource.State)
          else
            Result := Format('%s: %s', [DataSetName, GetCaption(DataSource.State)]);
        end
        else { doGlyph }
          Result := '';
      lsRecordNo:
        if FDataLink.Active then
        begin
          if FRecordNo >= 0 then
          begin
            if FRecordCount >= 0 then
              Result := Format('%d:%d', [FRecordNo, FRecordCount])
            else
              Result := IntToStr(FRecordNo);
          end
          else
          begin
            if FRecordCount >= 0 then
              Result := Format('( %d )', [FRecordCount])
            else
              Result := '';
          end;
        end
        else
          Result := '';
      lsRecordSize:
        if FDataLink.Active then
          Result := IntToStr(FDataLink.DataSet.RecordSize)
        else
          Result := '';
    end;
  end;
end;

function TJvDBStatusLabel.GetDatasetState: TDataSetState;
begin
  if DataSource <> nil then
    Result := DataSource.State
  else
    Result := dsInactive;
end;

procedure TJvDBStatusLabel.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  if csDesigning in ComponentState then
    Invalidate;
end;

function TJvDBStatusLabel.GetCaptions: TStrings;
begin
  Result := FCaptions;
end;

procedure TJvDBStatusLabel.SetCaptions(Value: TStrings);
begin
  FCaptions.Assign(Value);
end;

function TJvDBStatusLabel.GetStatusKind(State: TDataSetState): TDBStatusKind;
begin
  if not (State in [Low(TDBStatusKind)..High(TDBStatusKind)]) then
  begin
    case State of
      dsFilter:
        Result := dsSetKey;
      dsNewValue, dsOldValue, dsCurValue:
        Result := dsEdit;
    else
      Result := TDBStatusKind(State);
    end;
  end
  else
    Result := TDBStatusKind(State);
end;

function TJvDBStatusLabel.GetCaption(State: TDataSetState): string;
const
  StrIds: array [TDBStatusKind] of string = (RsInactiveData, RsBrowseData,
    RsEditData, RsInsertData, RsSetKeyData, RsCalcFieldsData);
var
  Kind: TDBStatusKind;
begin
  Kind := GetStatusKind(State);
  if (Ord(Kind) < Captions.Count) and (Captions[Ord(Kind)] <> '') then
    Result := Captions[Ord(Kind)]
  else
    Result := StrIds[Kind];
end;

procedure TJvDBStatusLabel.Paint;
var
  GlyphOrigin: TPoint;
begin
  inherited Paint;
  if (FStyle = lsState) and (FShowOptions in [doGlyph, doBoth]) and
    (FCell <> nil) then
  begin
    if GlyphAlign = glGlyphLeft then
      GlyphOrigin.X := GlyphSpacing
    else {glGlyphRight}
      GlyphOrigin.X := ClientWidth - MarginRight + GlyphSpacing;
    case Layout of
      tlTop:
        GlyphOrigin.Y := 0;
      tlCenter:
        GlyphOrigin.Y := (ClientHeight - FCell.Height) div 2;
    else { tlBottom }
      GlyphOrigin.Y := ClientHeight - FCell.Height;
    end;
    DrawBitmapTransparent(Canvas, GlyphOrigin.X, GlyphOrigin.Y,
      FCell, FGlyph.TransparentColor);
  end;
end;

procedure TJvDBStatusLabel.CaptionsChanged(Sender: TObject);
begin
  FCaptions.OnChange := nil;
  try
    while (Pred(FCaptions.Count) > Ord(High(TDBStatusKind))) do
      FCaptions.Delete(FCaptions.Count - 1);
  finally
    FCaptions.OnChange := CaptionsChanged;
  end;
  if not (csDesigning in ComponentState) then
    Invalidate;
end;

procedure TJvDBStatusLabel.UpdateData;

  function IsSequenced: Boolean;
  begin
    Result := FDataLink.DataSet.IsSequenced;
  end;

begin
  FRecordCount := -1;
  if (FStyle = lsRecordNo) and FDataLink.Active and
    (DataSource.State in [dsBrowse, dsEdit]) then
  begin
    if Assigned(FOnGetRecordCount) then
      FOnGetRecordCount(Self, FDataLink.DataSet, FRecordCount)
    else
    if FCalcCount or IsSequenced then
      FRecordCount := FDataLink.DataSet.RecordCount;
  end;
  UpdateStatus;
end;

procedure TJvDBStatusLabel.UpdateStatus;
begin
  if DataSource <> nil then
  begin
    case FStyle of
      lsState:
        if FShowOptions in [doGlyph, doBoth] then
        begin
          if GlyphAlign = glGlyphLeft then
          begin
            if AutoSize then
              Alignment := taRightJustify;
            MarginRight := 0;
            MarginLeft := (FGlyph.Width div GlyphColumns) + GlyphSpacing * 2;
          end
          else {glGlyphRight}
          begin
            if AutoSize then
              Alignment := taLeftJustify;
            MarginLeft := 0;
            MarginRight := (FGlyph.Width div GlyphColumns) + GlyphSpacing * 2;
          end;
          if FCell = nil then
            FCell := TBitmap.Create;
          AssignBitmapCell(FGlyph, FCell, GlyphColumns, 1,
            Ord(GetStatusKind(DataSource.State)));
        end
        else { doCaption }
        begin
          FCell.Free;
          FCell := nil;
          MarginLeft := 0;
          MarginRight := 0;
        end;
      lsRecordNo:
        begin
          FCell.Free;
          FCell := nil;
          MarginLeft := 0;
          MarginRight := 0;
          FRecordNo := -1;
          if FDataLink.Active then
          begin
            if Assigned(FOnGetRecNo) then
              FOnGetRecNo(Self, FDataLink.DataSet, FRecordNo)
            else
            try
              with FDataLink.DataSet do
                if not IsEmpty then
                  FRecordNo := RecNo;
            except
            end;
          end;
        end;
      lsRecordSize:
        begin
          FCell.Free;
          FCell := nil;
          MarginLeft := 0;
          MarginRight := 0;
        end;
    end;
  end
  else
  begin
    FCell.Free;
    FCell := nil;
  end;
  NeedsResize := True;
  AdjustBounds;
  Invalidate;
end;

procedure TJvDBStatusLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then
    DataSource := nil;
end;

function TJvDBStatusLabel.GetDataSetName: string;
begin
  Result := FDataSetName;
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FOnGetDataName) then
      Result := FOnGetDataName(Self)
    else
    if (Result = '') and (DataSource <> nil) and (DataSource.DataSet <> nil) then
      Result := DataSource.DataSet.Name;
  end;
end;

procedure TJvDBStatusLabel.SetDataSetName(Value: string);
begin
  FDataSetName := Value;
  Invalidate;
end;

function TJvDBStatusLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBStatusLabel.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
  begin
    if FDataLink.DataSource <> nil then
      FDataLink.DataSource.RemoveFreeNotification(Self);
    FDataLink.DataSource := Value;
  end;
  if Value <> nil then
    Value.FreeNotification(Self);
  if not (csLoading in ComponentState) then
    UpdateData;
end;

procedure TJvDBStatusLabel.SetEditColor(Value: TColor);
begin
  if FEditColor <> Value then
  begin
    FEditColor := Value;
    if Style = lsState then
      Invalidate;
  end;
end;

procedure TJvDBStatusLabel.SetGlyphAlign(Value: TGlyphAlign);
begin
  if FGlyphAlign <> Value then
  begin
    FGlyphAlign := Value;
    UpdateStatus;
  end;
end;

procedure TJvDBStatusLabel.SetShowOptions(Value: TDBLabelOptions);
begin
  if FShowOptions <> Value then
  begin
    FShowOptions := Value;
    UpdateStatus;
  end;
end;

procedure TJvDBStatusLabel.SetCalcCount(Value: Boolean);
begin
  if FCalcCount <> Value then
  begin
    FCalcCount := Value;
    if not (csLoading in ComponentState) then
      UpdateData;
  end;
end;

procedure TJvDBStatusLabel.SetStyle(Value: TDBLabelStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if not (csLoading in ComponentState) then
      UpdateData;
  end;
end;

//=== { TJvDBNavigator } =====================================================

constructor TJvDBNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Transparent := True;
end;

procedure TJvDBNavigator.SetTransparent(Value: Boolean);
var
  Button: TNavigateBtn;
begin
  FTransparent := Value;
  if Value then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];

  {$IFDEF COMPILER7_UP}
  ParentBackground := Value;
  {$ENDIF COMPILER7_UP}
  for Button := Low(Buttons) to High(Buttons) do
    Buttons[Button].Transparent := Value;
  Invalidate;
end;

procedure TJvDBNavigator.Paint;
begin
  if Transparent then
    Exit;
end;

procedure TJvDBNavigator.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  Pt: TPoint;
begin
  if Transparent then
  begin
    OffsetWindowOrgEx(Msg.DC, Left, Top, @Pt);
    SendMessage(Parent.Handle, WM_ERASEBKGND, Msg.DC, Msg.DC);
    SendMessage(Parent.Handle, WM_PRINTCLIENT, Msg.DC, Msg.DC);
    SetWindowOrgEx(Msg.DC, Pt.X, Pt.Y, nil);
    Msg.Result := 1;
  end
  else
    inherited;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
