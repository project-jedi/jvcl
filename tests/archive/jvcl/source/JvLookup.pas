{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLookup.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Polaris Software

Copyright (c) 1995,1997 Borland International
Portions copyright (c) 1995, 1996 AO ROSNO
Portions copyright (c) 1997, 1998 Master-Bank

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvLookup;

interface

uses
  SysUtils, Windows, DBCtrls,
  {$IFDEF COMPILER6_UP}
  Variants, VDBConsts,
  {$ENDIF}
  Messages, Classes, Controls, Forms, Graphics, Menus, DB, Mask, StdCtrls,
  {$IFNDEF COMPILER3_UP}
  DBTables,
  {$ENDIF}
  JvDBUtils, JvToolEdit;

const
  // (rom) renamed
  DefFieldsDelimiter = ',';

type
  TLookupListStyle = (lsFixed, lsDelimited);
  TJvLookupControl = class;
  TGetImageEvent = procedure(Sender: TObject; IsEmpty: Boolean;
    var Graphic: TGraphic; var TextMargin: Integer) of object;

  TJvDataSourceLink = class(TDataLink)
  private
    FDataControl: TJvLookupControl;
  protected
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure FocusControl(Field: TFieldRef); override;
    procedure RecordChanged(Field: TField); override;
  end;

  TLookupSourceLink = class(TDataLink)
  private
    FDataControl: TJvLookupControl;
  protected
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure DataSetChanged; override;
  end;

  TJvLookupControl = class(TCustomControl)
  private
    FLookupSource: TDataSource;
    FDataLink: TJvDataSourceLink;
    FLookupLink: TLookupSourceLink;
    FDataFieldName: string;
    FLookupFieldName: string;
    FLookupDisplay: string;
    FDisplayIndex: Integer;
    FDataField: TField;
    FMasterField: TField;
    FKeyField: TField;
    FDisplayField: TField;
    FListFields: TList;
    FValue: string;
    FDisplayValue: string;
    FDisplayEmpty: string;
    FSearchText: string;
    FEmptyValue: string;
    FEmptyStrIsNull: Boolean; // Polaris
    FEmptyItemColor: TColor;
    FListActive: Boolean;
    FPopup: Boolean;
    FFocused: Boolean;
    FLocate: TJvLocateObject;
    FIndexSwitch: Boolean;
    FIgnoreCase: Boolean;
    FItemHeight: Integer;
    FFieldsDelimiter: Char;
    FListStyle: TLookupListStyle;
    FLookupFormat: string;
    FOnChange: TNotifyEvent;
    FOnGetImage: TGetImageEvent;
    {$IFDEF WIN32}
    FLookupMode: Boolean;
    procedure CheckNotFixed;
    procedure SetLookupMode(Value: Boolean);
    function GetKeyValue: Variant;
    procedure SetKeyValue(const Value: Variant);
    {$ENDIF}
    function CanModify: Boolean;
    procedure CheckNotCircular;
    procedure DataLinkActiveChanged;
    procedure CheckDataLinkActiveChanged;
    procedure DataLinkRecordChanged(Field: TField);
    function GetBorderSize: Integer;
    function GetField: TField;
    function GetDataSource: TDataSource;
    function GetLookupField: string;
    function GetLookupSource: TDataSource;
    function GetReadOnly: Boolean;
    function GetTextHeight: Integer;
    function DefaultTextHeight: Integer;
    function GetItemHeight: Integer;
    function LocateKey: Boolean;
    function LocateDisplay: Boolean;
    function ValueIsEmpty(const S: string): Boolean;
    function StoreEmpty: Boolean;
    procedure ProcessSearchKey(Key: Char);
    procedure UpdateKeyValue;
    procedure SelectKeyValue(const Value: string);
    procedure SetDataFieldName(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetDisplayEmpty(const Value: string);
    procedure SetEmptyValue(const Value: string);
    procedure SetEmptyStrIsNull(const Value: Boolean); // Polaris
    procedure SetEmptyItemColor(Value: TColor);
    procedure SetLookupField(const Value: string);
    procedure SetValueKey(const Value: string);
    procedure SetValue(const Value: string);
    procedure SetDisplayValue(const Value: string);
    procedure SetListStyle(Value: TLookupListStyle); virtual;
    procedure SetFieldsDelimiter(Value: Char); virtual;
    procedure SetLookupDisplay(const Value: string);
    procedure SetLookupFormat(const Value: string);
    procedure SetLookupSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetItemHeight(Value: Integer);
    function ItemHeightStored: Boolean;
    procedure DrawPicture(Canvas: TCanvas; Rect: TRect; Image: TGraphic);
    procedure UpdateDisplayValue;
    function EmptyRowVisible: Boolean;
    procedure WMGetDlgCode(var Msg: TMessage); message WM_GETDLGCODE;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
  protected
    procedure Change; dynamic;
    procedure KeyValueChanged; virtual;
    procedure DisplayValueChanged; virtual;
    function DoFormatLine: string;
    procedure ListLinkActiveChanged; virtual;
    procedure ListLinkDataChanged; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function GetPicture(Current, Empty: Boolean; var TextMargin: Integer): TGraphic; virtual;
    procedure UpdateDisplayEmpty(const Value: string); virtual;
    function SearchText(var AValue: string): Boolean;
    function GetWindowWidth: Integer;
    property DataField: string read FDataFieldName write SetDataFieldName;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DisplayEmpty: string read FDisplayEmpty write SetDisplayEmpty;
    property EmptyValue: string read FEmptyValue write SetEmptyValue stored StoreEmpty;
    property EmptyStrIsNull: Boolean read FEmptyStrIsNull write SetEmptyStrIsNull default True; // Polaris
    property EmptyItemColor: TColor read FEmptyItemColor write SetEmptyItemColor default clWindow;
    property IgnoreCase: Boolean read FIgnoreCase write FIgnoreCase default True;
    property IndexSwitch: Boolean read FIndexSwitch write FIndexSwitch default True;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight
      stored ItemHeightStored;
    property ListStyle: TLookupListStyle read FListStyle write SetListStyle default lsFixed;
    property FieldsDelimiter: Char read FFieldsDelimiter write SetFieldsDelimiter default DefFieldsDelimiter;
    property LookupDisplay: string read FLookupDisplay write SetLookupDisplay;
    property LookupDisplayIndex: Integer read FDisplayIndex write FDisplayIndex default 0;
    property LookupField: string read GetLookupField write SetLookupField;
    property LookupFormat: string read FLookupFormat write SetLookupFormat;
    property LookupSource: TDataSource read GetLookupSource write SetLookupSource;
    property ParentColor default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property TabStop default True;
    property Value: string read FValue write SetValue stored False;
    property DisplayValue: string read FDisplayValue write SetDisplayValue stored False;
    {$IFDEF WIN32}
    property KeyValue: Variant read GetKeyValue write SetKeyValue stored False;
    {$ENDIF}
    procedure SetFieldValue(Field: TField; const Value: string); // Polaris
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetImage: TGetImageEvent read FOnGetImage write FOnGetImage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearValue;
    function Locate(const SearchField: TField; const AValue: string;
      Exact: Boolean): Boolean;
    procedure ResetField; virtual;
    {$IFDEF COMPILER4_UP}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    {$ENDIF}
    property Field: TField read GetField;
  end;

  TJvDBLookupList = class(TJvLookupControl)
  private
    FRecordIndex: Integer;
    FRecordCount: Integer;
    FRowCount: Integer;
    FBorderStyle: TBorderStyle;
    FKeySelected: Boolean;
    FTracking: Boolean;
    FTimerActive: Boolean;
    FLockPosition: Boolean;
    FSelectEmpty: Boolean;
    FMousePos: Integer;
    function GetKeyIndex: Integer;
    procedure ListDataChanged;
    procedure SelectCurrent;
    procedure SelectItemAt(X, Y: Integer);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetRowCount(Value: Integer);
    procedure StopTimer;
    procedure StopTracking;
    procedure TimerScroll;
    procedure UpdateScrollBar;
    procedure UpdateBufferCount(Rows: Integer);
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure WMCancelMode(var Msg: TMessage); message WM_CANCELMODE;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMTimer(var Msg: TMessage); message WM_TIMER;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyValueChanged; override;
    procedure DisplayValueChanged; override;
    procedure ListLinkActiveChanged; override;
    procedure ListLinkDataChanged; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure UpdateDisplayEmpty(const Value: string); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DrawItemText(Canvas: TCanvas; Rect: TRect;
      Selected, IsEmpty: Boolean); virtual;
    property RowCount: Integer read FRowCount write SetRowCount stored False;
    property DisplayValue;
    property Value;
    {$IFDEF WIN32}
    property KeyValue;
    {$ENDIF}
  published
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Align;
    property Color;
    property Ctl3D;
    property DataField;
    property DataSource;
    property DisplayEmpty;
    property DragCursor;
    property DragMode;
    property EmptyItemColor;
    property EmptyValue;
    property EmptyStrIsNull; // Polaris
    property Enabled;
    property FieldsDelimiter;
    property Font;
    property IgnoreCase;
    {$IFDEF COMPILER4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    {$ENDIF}
    {$IFDEF WIN32}
    {$IFDEF COMPILER3_UP}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    {$ENDIF}
    property IndexSwitch;
    property ItemHeight;
    property ListStyle;
    property LookupField;
    property LookupDisplay;
    property LookupDisplayIndex;
    property LookupFormat;
    property LookupSource;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImage;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF WIN32}
    property OnStartDrag;
    {$ENDIF}
    {$IFDEF COMPILER5_UP}
    property OnContextPopup;
    {$ENDIF}
    {$IFDEF COMPILER4_UP}
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
  end;

  TJvPopupDataList = class(TJvDBLookupList)
  private
    FCombo: TJvLookupControl;
    procedure WMMouseActivate(var Msg: TMessage); message WM_MOUSEACTIVATE;
  protected
    procedure Click; override;
    procedure CreateParams(var Params: TCreateParams); override;
    {$IFNDEF WIN32}
    procedure CreateWnd; override;
    {$ENDIF}
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFNDEF WIN32}
  TDropDownAlign = (daLeft, daRight, daCenter);
  {$ENDIF}

  TJvDBLookupCombo = class(TJvLookupControl)
  private
    FDataList: TJvPopupDataList;
    FButtonWidth: Integer;
    FDropDownCount: Integer;
    FDropDownWidth: Integer;
    FDropDownAlign: TDropDownAlign;
    FEscapeClear: Boolean;
    FListVisible: Boolean;
    FPressed: Boolean;
    FTracking: Boolean;
    FAlignment: TAlignment;
    FSelImage: TPicture;
    FSelMargin: Integer;
    FDisplayValues: TStrings;
    FDisplayAllFields: Boolean;
    {$IFNDEF WIN32}
    FBtnGlyph: TBitmap;
    FBtnDisabled: TBitmap;
    {$ENDIF}
    {$IFDEF JVCLThemesEnabled}
    FOver: Boolean;
    {$ENDIF}
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StopTracking;
    procedure TrackButton(X, Y: Integer);
    function GetMinHeight: Integer;
    function GetText: string;
    procedure InvalidateText;
    procedure UpdateCurrentImage;
    procedure PaintDisplayValues(Canvas: TCanvas; R: TRect; ALeft: Integer);
    procedure SetFieldsDelimiter(Value: Char); override;
    procedure SetListStyle(Value: TLookupListStyle); override;
    function GetDisplayAllFields: Boolean;
    procedure SetDisplayAllFields(Value: Boolean);
    function GetDisplayValues(Index: Integer): string;
    procedure CMCancelMode(var Msg: TCMCancelMode); message CM_CANCELMODE;
    {$IFDEF WIN32}
    procedure CNKeyDown(var Msg: TWMKeyDown); message CN_KEYDOWN;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
    {$ENDIF}
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    {$IFDEF JVCLThemesEnabled}
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    {$ENDIF}
    procedure WMCancelMode(var Msg: TMessage); message WM_CANCELMODE;
    procedure WMGetDlgCode(var Msg: TMessage); message WM_GETDLGCODE;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    {$IFDEF COMPILER4_UP}
    procedure CMBiDiModeChanged(var Msg: TMessage); message CM_BIDIMODECHANGED;
    {$ENDIF}
  protected
    procedure Click; override;
    procedure CreateParams(var Params: TCreateParams); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function GetPicture(Current, Empty: Boolean; var TextMargin: Integer): TGraphic; override;
    procedure UpdateFieldText;
    procedure KeyValueChanged; override;
    procedure DisplayValueChanged; override;
    procedure ListLinkActiveChanged; override;
    procedure ListLinkDataChanged; override;
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure UpdateDisplayEmpty(const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseUp(Accept: Boolean); dynamic;
    procedure DropDown; virtual;
    procedure ResetField; override;
    property IsDropDown: Boolean read FListVisible;
    property ListVisible: Boolean read FListVisible;
    property Text: string read GetText;
    property DisplayValue;
    property DisplayValues[Index: Integer]: string read GetDisplayValues;
    property Value;
    {$IFDEF WIN32}
    property KeyValue;
    {$ENDIF}
  published
    property Align; // Polaris
    property DropDownAlign: TDropDownAlign read FDropDownAlign write FDropDownAlign default daLeft;
    property DropDownCount: Integer read FDropDownCount write FDropDownCount default 7;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth default 0;
    property EscapeClear: Boolean read FEscapeClear write FEscapeClear default True;
    property DisplayAllFields: Boolean read GetDisplayAllFields write SetDisplayAllFields default False;
    property Color;
    property Ctl3D;
    property DataField;
    property DataSource;
    property DisplayEmpty;
    property DragCursor;
    property DragMode;
    property EmptyValue;
    property EmptyStrIsNull; // Polaris
    property EmptyItemColor;
    property Enabled;
    property FieldsDelimiter;
    property Font;
    property IgnoreCase;
    {$IFDEF COMPILER4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    {$ENDIF}
    {$IFDEF WIN32}
    {$IFDEF COMPILER3_UP}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    {$ENDIF}
    property IndexSwitch;
    property ItemHeight;
    property ListStyle;
    property LookupField;
    property LookupDisplay;
    property LookupDisplayIndex;
    property LookupFormat;
    property LookupSource;
    property ParentColor;
    property ParentCtl3D;
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
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImage;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$IFDEF WIN32}
    property OnStartDrag;
    {$ENDIF}
    {$IFDEF COMPILER5_UP}
    property OnContextPopup;
    {$ENDIF}
    {$IFDEF COMPILER4_UP}
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
  end;

  TJvPopupDataWindow = class(TJvPopupDataList)
  private
    FEditor: TWinControl;
    FCloseUp: TCloseUpEvent;
  protected
    procedure InvalidateEditor;
    procedure Click; override;
    procedure DisplayValueChanged; override;
    function GetPicture(Current, Empty: Boolean; var TextMargin: Integer): TGraphic; override;
    procedure KeyPress(var Key: Char); override;
    procedure PopupMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CloseUp(Accept: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Hide;
    procedure Show(Origin: TPoint);
    property OnCloseUp: TCloseUpEvent read FCloseUp write FCloseUp;
  end;

  TJvLookupEdit = class(TJvCustomComboEdit)
  private
    FChanging: Boolean;
    FIgnoreChange: Boolean;
    FDropDownCount: Integer;
    FDropDownWidth: Integer;
    FPopupOnlyLocate: Boolean;
    FOnCloseUp: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    function GetListStyle: TLookupListStyle;
    procedure SetListStyle(Value: TLookupListStyle);
    function GetFieldsDelimiter: Char;
    procedure SetFieldsDelimiter(Value: Char);
    function GetLookupDisplay: string;
    procedure SetLookupDisplay(const Value: string);
    function GetDisplayIndex: Integer;
    procedure SetDisplayIndex(Value: Integer);
    function GetLookupField: string;
    procedure SetLookupField(const Value: string);
    function GetLookupSource: TDataSource;
    procedure SetLookupSource(Value: TDataSource);
    procedure SetDropDownCount(Value: Integer);
    function GetLookupValue: string;
    procedure SetLookupValue(const Value: string);
    function GetOnGetImage: TGetImageEvent;
    procedure SetOnGetImage(Value: TGetImageEvent);
  protected
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure ShowPopup(Origin: TPoint); override;
    procedure HidePopup; override;
    procedure PopupChange; override;
    procedure PopupDropDown(DisableEdit: Boolean); override;
    {$IFDEF WIN32}
    function AcceptPopup(var Value: Variant): Boolean; override;
    procedure SetPopupValue(const Value: Variant); override;
    function GetPopupValue: Variant; override;
    {$ELSE}
    function AcceptPopup(var Value: string): Boolean; override;
    procedure SetPopupValue(const Value: string); override;
    function GetPopupValue: string; override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LookupValue: string read GetLookupValue write SetLookupValue;
  published
    property DropDownCount: Integer read FDropDownCount write SetDropDownCount default 8;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth default 0;
    property ListStyle: TLookupListStyle read GetListStyle write SetListStyle default lsFixed;
    property FieldsDelimiter: Char read GetFieldsDelimiter write SetFieldsDelimiter default DefFieldsDelimiter;
    property LookupDisplay: string read GetLookupDisplay write SetLookupDisplay;
    property LookupDisplayIndex: Integer read GetDisplayIndex write SetDisplayIndex default 0;
    property LookupField: string read GetLookupField write SetLookupField;
    property LookupSource: TDataSource read GetLookupSource write SetLookupSource;
    property PopupOnlyLocate: Boolean read FPopupOnlyLocate write FPopupOnlyLocate default True;
    property Alignment;
    property AutoSelect;
    property BorderStyle;
    property ButtonHint;
    property CharCase;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property HideSelection;
    {$IFDEF COMPILER4_UP}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    {$ENDIF}
    {$IFDEF WIN32}
    {$IFDEF COMPILER3_UP}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    {$ENDIF}
    property MaxLength;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupAlign;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnGetImage: TGetImageEvent read GetOnGetImage write SetOnGetImage;
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
    {$IFDEF WIN32}
    property OnStartDrag;
    {$ENDIF}
    {$IFDEF COMPILER5_UP}
    property OnContextPopup;
    {$ENDIF}
    {$IFDEF COMPILER4_UP}
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
  end;

implementation

uses
  DBConsts, Dialogs, Math,
  {$IFDEF JVCLThemesEnabled}
  Themes,
  {$ENDIF}
  {$IFNDEF WIN32}
  JvStr16,
  {$ENDIF}
  {$IFNDEF COMPILER3_UP}
  JvBdeUtils,
  {$ENDIF}
  JvVCLUtils, JvClipIcon;

procedure CheckLookupFormat(const AFormat: string);
var
  P: PChar;
begin
  { AFormat is passed to a Format function, but the only allowed
    format specifiers are %s, %S and %% }
  P := StrScan(PChar(AFormat), '%');
  while Assigned(P) do
  begin
    Inc(P);
    if P^ = #0 then
      raise Exception.Create('Invalid format: % not allowed')
    else
    if not (P^ in ['%', 's', 'S']) then
      raise Exception.Create(Format('Invalid format: %s not allowed',
        [QuotedStr('%' + P^)]));
    P := StrScan(P + 1, '%');
  end;
end;

function GetSpecifierCount(const AFormat: string): Integer;
var
  P: PChar;
begin
  { GetSpecifierCount counts the nr of format specifiers in AFormat }
  Result := 0;
  P := StrScan(PChar(AFormat), '%');
  while Assigned(P) do
  begin
    Inc(P);
    if P^ = #0 then
      Exit
    else
    if P^ in ['s', 'S'] then
      Inc(Result);
    P := StrScan(P + 1, '%');
  end;
end;

//=== TJvDataSourceLink ======================================================

procedure TJvDataSourceLink.ActiveChanged;
begin
  if FDataControl <> nil then
    FDataControl.DataLinkActiveChanged;
end;

procedure TJvDataSourceLink.LayoutChanged;
begin
  if FDataControl <> nil then
    FDataControl.CheckDataLinkActiveChanged;
end;

procedure TJvDataSourceLink.RecordChanged(Field: TField);
begin
  if FDataControl <> nil then
    FDataControl.DataLinkRecordChanged(Field);
end;

procedure TJvDataSourceLink.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (FDataControl <> nil) and
    (Field^ = FDataControl.FDataField) and FDataControl.CanFocus then
  begin
    Field^ := nil;
    FDataControl.SetFocus;
  end;
end;

//=== TLookupSourceLink ======================================================

procedure TLookupSourceLink.ActiveChanged;
begin
  if FDataControl <> nil then
    FDataControl.ListLinkActiveChanged;
end;

procedure TLookupSourceLink.LayoutChanged;
begin
  if FDataControl <> nil then
    FDataControl.ListLinkActiveChanged;
end;

procedure TLookupSourceLink.DataSetChanged;
begin
  if FDataControl <> nil then
    FDataControl.ListLinkDataChanged;
end;

//=== TJvLookupControl =======================================================

// (rom) changed to var
var
  SearchTickCount: Longint = 0;

{$IFNDEF WIN32}
procedure GetFieldList(DataSet: TDataSet; List: TList; const FieldNames: string);
var
  Pos: Integer;
begin
  Pos := 1;
  while Pos <= Length(FieldNames) do
    List.Add(DataSet.FieldByName(ExtractFieldName(FieldNames, Pos)));
end;
{$ENDIF}

constructor TJvLookupControl.Create(AOwner: TComponent);
const
  {$IFDEF JVCLThemesEnabled}
  LookupStyle = [csOpaque, csNeedsBorderPaint];
  {$ELSE}
  LookupStyle = [csOpaque];
  {$ENDIF}
begin
  inherited Create(AOwner);
  if NewStyleControls then
    ControlStyle := LookupStyle
  else
    ControlStyle := LookupStyle + [csFramed];
  ParentColor := False;
  TabStop := True;
  FFieldsDelimiter := DefFieldsDelimiter;
  FLookupSource := TDataSource.Create(Self);
  FDataLink := TJvDataSourceLink.Create;
  FDataLink.FDataControl := Self;
  FLookupLink := TLookupSourceLink.Create;
  FLookupLink.FDataControl := Self;
  FListFields := TList.Create;
  FEmptyValue := EmptyStr;
  FEmptyStrIsNull := True; // Polaris
  FEmptyItemColor := clWindow;
  FValue := FEmptyValue;
  {$IFDEF COMPILER3_UP}
  FLocate := CreateLocate(nil);
  {$ELSE}
  FLocate := TJvDBLocate.Create;
  {$ENDIF}
  FIndexSwitch := True;
  FIgnoreCase := True;
end;

destructor TJvLookupControl.Destroy;
begin
  FListFields.Free;
  FListFields := nil;
  if FLookupLink <> nil then
    FLookupLink.FDataControl := nil;
  FLookupLink.Free;
  FLookupLink := nil;
  if FDataLink <> nil then
    FDataLink.FDataControl := nil;
  FDataLink.Free;
  FDataLink := nil;
  FLocate.Free;
  FLocate := nil;
  inherited Destroy;
end;

function TJvLookupControl.CanModify: Boolean;
begin
  Result := FListActive and not ReadOnly and ((FDataLink.DataSource = nil) or
    (FMasterField <> nil) and FMasterField.CanModify);
end;

procedure TJvLookupControl.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TJvLookupControl.ValueIsEmpty(const S: string): Boolean;
begin
  Result := (S = FEmptyValue);
end;

function TJvLookupControl.StoreEmpty: Boolean;
begin
  Result := (FEmptyValue <> EmptyStr);
end;

{$IFDEF WIN32}

procedure TJvLookupControl.CheckNotFixed;
begin
  if FLookupMode then
    _DBError(SPropDefByLookup);
  if FDataLink.DataSourceFixed then
    _DBError(SDataSourceFixed);
end;

procedure TJvLookupControl.SetLookupMode(Value: Boolean);
begin
  if FLookupMode <> Value then
    if Value then
    begin
      FMasterField := FDataField.DataSet.FieldByName(FDataField.KeyFields);
      FLookupSource.DataSet := FDataField.LookupDataSet;
      FLookupFieldName := FDataField.LookupKeyFields;
      FLookupMode := True;
      FLookupLink.DataSource := FLookupSource;
    end
    else
    begin
      FLookupLink.DataSource := nil;
      FLookupMode := False;
      FLookupFieldName := '';
      FLookupSource.DataSet := nil;
      FMasterField := FDataField;
    end;
end;

function TJvLookupControl.GetKeyValue: Variant;
begin
  { (rb) EmptyStr is provided for backwards compatibility only in D7 }
  if ValueIsEmpty(Value) then
    if (Value = EmptyStr) and FEmptyStrIsNull then
      Result := Null // Polaris
    else
      Result := FEmptyValue // Polaris
  else
    Result := Value;
end;

procedure TJvLookupControl.SetKeyValue(const Value: Variant);
begin
  if VarIsNull(Value) then
    Self.Value := FEmptyValue // Polaris
  else
    Self.Value := Value;
  //  Self.Value := Value;
end;

{$ENDIF}

procedure TJvLookupControl.CheckNotCircular;
begin
  {
  if FDataLink.Active and FDataLink.DataSet.IsLinkedTo(LookupSource) then
    _DBError(SCircularDataLink);
  }
  if FDataLink.Active and ((DataSource = LookupSource) or
    (FDataLink.DataSet = FLookupLink.DataSet)) then
    _DBError(SCircularDataLink);
end;

procedure TJvLookupControl.CheckDataLinkActiveChanged;
var
  TestField: TField;
begin
  if FDataLink.Active and (FDataFieldName <> '') then
  begin
    TestField := FDataLink.DataSet.FieldByName(FDataFieldName);
    if Pointer(FDataField) <> Pointer(TestField) then
    begin
      FDataField := nil;
      FMasterField := nil;
      CheckNotCircular;
      FDataField := TestField;
      FMasterField := FDataField;
      DataLinkRecordChanged(nil);
    end;
  end;
end;

procedure TJvLookupControl.DataLinkActiveChanged;
begin
  FDataField := nil;
  FMasterField := nil;
  if FDataLink.Active and (FDataFieldName <> '') then
  begin
    CheckNotCircular;
    FDataField := FDataLink.DataSet.FieldByName(FDataFieldName);
    FMasterField := FDataField;
  end;
  {$IFDEF WIN32}
  SetLookupMode((FDataField <> nil) and FDataField.Lookup);
  {$ENDIF}
  DataLinkRecordChanged(nil);
end;

procedure TJvLookupControl.DataLinkRecordChanged(Field: TField);
begin
  if (Field = nil) or (Field = FMasterField) then
  begin
    if FMasterField <> nil then
    begin
      SetValueKey(FMasterField.AsString);
    end
    else
      SetValueKey(FEmptyValue);
  end;
end;

{$IFDEF COMPILER4_UP}

function TJvLookupControl.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or ((FDataLink <> nil) and
    FDataLink.ExecuteAction(Action));
end;

function TJvLookupControl.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or ((FDataLink <> nil) and
    FDataLink.UpdateAction(Action));
end;

function TJvLookupControl.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

{$ENDIF}

function TJvLookupControl.GetBorderSize: Integer;
var
  Params: TCreateParams;
  R: TRect;
begin
  CreateParams(Params);
  SetRect(R, 0, 0, 0, 0);
  {$IFDEF WIN32}
  AdjustWindowRectEx(R, Params.Style, False, Params.ExStyle);
  {$ELSE}
  AdjustWindowRect(R, Params.Style, False);
  if (csFramed in ControlStyle) and Ctl3D and
    (Params.Style and WS_BORDER <> 0) then
    Inc(R.Bottom, 2);
  {$ENDIF}
  Result := R.Bottom - R.Top;
end;

function TJvLookupControl.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TJvLookupControl.GetLookupField: string;
begin
  {$IFDEF WIN32}
  if FLookupMode then
    Result := ''
  else
  {$ENDIF}
    Result := FLookupFieldName;
end;

function TJvLookupControl.GetLookupSource: TDataSource;
begin
  {$IFDEF WIN32}
  if FLookupMode then
    Result := nil
  else
  {$ENDIF}
    Result := FLookupLink.DataSource;
end;

function TJvLookupControl.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

function TJvLookupControl.GetField: TField;
begin
  if Assigned(FDataLink) then
    Result := FDataField
  else
    Result := nil;
end;

// (rom) is this useful for other ocmponents? It seems superior.

function TJvLookupControl.DefaultTextHeight: Integer;
var
  DC: HDC;
  SaveFont: HFONT;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight;
end;

function TJvLookupControl.GetTextHeight: Integer;
begin
  Result := Max(DefaultTextHeight, FItemHeight);
end;

procedure TJvLookupControl.KeyValueChanged;
begin
end;

procedure TJvLookupControl.DisplayValueChanged;
begin
end;

procedure TJvLookupControl.ListLinkActiveChanged;
var
  DataSet: TDataSet;
  {$IFDEF WIN32}
  ResultField: TField;
  {$ENDIF}
begin
  FListActive := False;
  FKeyField := nil;
  FDisplayField := nil;
  FListFields.Clear;
  if FLookupLink.Active and (FLookupFieldName <> '') then
  begin
    CheckNotCircular;
    DataSet := FLookupLink.DataSet;
    FKeyField := DataSet.FieldByName(FLookupFieldName);
    {$IFDEF WIN32}
    DataSet.GetFieldList(FListFields, FLookupDisplay);
    {$ELSE}
    GetFieldList(DataSet, FListFields, FLookupDisplay);
    {$ENDIF}
    {$IFDEF WIN32}
    if FLookupMode then
    begin
      ResultField := DataSet.FieldByName(FDataField.LookupResultField);
      if FListFields.IndexOf(ResultField) < 0 then
        FListFields.Insert(0, ResultField);
      FDisplayField := ResultField;
    end
    else
    begin
      if FListFields.Count = 0 then
        FListFields.Add(FKeyField);
      if (FDisplayIndex >= 0) and (FDisplayIndex < FListFields.Count) then
        FDisplayField := FListFields[FDisplayIndex]
      else
        FDisplayField := FListFields[0];
    end;
    {$ELSE}
    if FListFields.Count = 0 then
      FListFields.Add(FKeyField);
    if (FDisplayIndex >= 0) and (FDisplayIndex < FListFields.Count) then
      FDisplayField := FListFields[FDisplayIndex]
    else
      FDisplayField := FListFields[0];
    {$ENDIF}

    { Reset LookupFormat if the number of specifiers > fields count
      else function Format will raise an error }
    if GetSpecifierCount(FLookupFormat) > FListFields.Count then
      FLookupFormat := '';

    FListActive := True;
  end;
  FLocate.DataSet := FLookupLink.DataSet;
end;

procedure TJvLookupControl.ListLinkDataChanged;
begin
end;

function TJvLookupControl.LocateDisplay: Boolean;
begin
  Result := False;
  try
    Result := Locate(FDisplayField, FDisplayValue, True);
  except
  end;
end;

function TJvLookupControl.LocateKey: Boolean;
begin
  Result := False;
  try
    Result := not ValueIsEmpty(FValue) and Locate(FKeyField, FValue, True);
  except
  end;
end;

procedure TJvLookupControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
    if (FLookupLink <> nil) and (AComponent = LookupSource) then
      LookupSource := nil;
  end;
end;

function TJvLookupControl.SearchText(var AValue: string): Boolean;
begin
  Result := False;
  if FDisplayField <> nil then
    if (AValue <> '') and Locate(FDisplayField, AValue, False) then
    begin
      SelectKeyValue(FKeyField.AsString);
      AValue := Copy(FDisplayField.AsString, 1, Length(AValue));
      Result := True;
    end
    else
    if AValue = '' then
    begin
      FLookupLink.DataSet.First;
      SelectKeyValue(FKeyField.AsString);
      AValue := '';
    end;
end;

procedure TJvLookupControl.ProcessSearchKey(Key: Char);
var
  TickCount: Longint;
  S: string;
begin
  S := '';
  if (FDisplayField <> nil) {and (FDisplayField.DataType = ftString)} then
    case Key of
      #9, #27:
        FSearchText := '';
      Char(VK_BACK), #32..#255:
        if CanModify then
        begin
          if not FPopup then
          begin
            TickCount := GetTickCount;
            if TickCount - SearchTickCount > 2000 then
              FSearchText := '';
            SearchTickCount := TickCount;
          end;
          if Key = Char(VK_BACK) then
            S := Copy(FSearchText, 1, Length(FSearchText) - 1)
          else
          if Length(FSearchText) < 32 then
            S := FSearchText + Key;
          if SearchText(S) or (S = '') then
            FSearchText := S;
        end;
    end;
end;

procedure TJvLookupControl.ResetField;
begin
  { if (FDataLink.DataSource = nil) or
    ((FDataLink.DataSource <> nil) and CanModify) then
  begin
    if (FDataLink.DataSource <> nil) and (FMasterField <> nil) and
      FDataLink.Edit then
    begin
      if FEmptyValue = EmptyStr then
        FMasterField.Clear
      else
        FMasterField.AsString := FEmptyValue;
                  end; }// Polaris
  if (FDataLink.DataSource = nil) or
    (FMasterField = nil) or FDataLink.Edit then
  begin
    if FDataLink.Edit then
      SetFieldValue(FMasterField, FEmptyValue); // Polaris
    FValue := FEmptyValue;
    FDisplayValue := EmptyStr;
    inherited Text := DisplayEmpty;
    Invalidate;
    Click;
  end;
end;

procedure TJvLookupControl.ClearValue;
begin
  SetValueKey(FEmptyValue);
end;

procedure TJvLookupControl.SelectKeyValue(const Value: string);
begin
  if FMasterField <> nil then
  begin
    if CanModify and FDataLink.Edit then
    begin
      if FDataField = FMasterField then
        FDataField.DataSet.Edit;
      // FMasterField.AsString := Value;
      SetFieldValue(FMasterField, Value); // Polaris
    end
    else
      Exit;
  end
  else
    SetValueKey(Value);
  UpdateDisplayValue;
  Repaint;
  Click;
end;

procedure TJvLookupControl.SetDataFieldName(const Value: string);
begin
  if FDataFieldName <> Value then
  begin
    FDataFieldName := Value;
    DataLinkActiveChanged;
  end;
end;

procedure TJvLookupControl.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  {$IFDEF WIN32}
  if Value <> nil then
    Value.FreeNotification(Self);
  {$ENDIF}
end;

procedure TJvLookupControl.SetListStyle(Value: TLookupListStyle);
begin
  if FListStyle <> Value then
  begin
    FListStyle := Value;
    Invalidate;
  end;
end;

procedure TJvLookupControl.SetFieldsDelimiter(Value: Char);
begin
  if FFieldsDelimiter <> Value then
  begin
    FFieldsDelimiter := Value;
    if ListStyle = lsDelimited then
      Invalidate;
  end;
end;

procedure TJvLookupControl.SetLookupField(const Value: string);
begin
  {$IFDEF WIN32}
  CheckNotFixed;
  {$ENDIF}
  if FLookupFieldName <> Value then
  begin
    FLookupFieldName := Value;
    ListLinkActiveChanged;
    if FListActive then
      DataLinkRecordChanged(nil);
  end;
end;

procedure TJvLookupControl.SetDisplayEmpty(const Value: string);
begin
  if FDisplayEmpty <> Value then
  begin
    UpdateDisplayEmpty(Value);
    FDisplayEmpty := Value;
    if not (csReading in ComponentState) then
      Invalidate;
  end;
end;

procedure TJvLookupControl.SetEmptyValue(const Value: string);
begin
  if FEmptyValue <> Value then
  begin
    if ValueIsEmpty(FValue) then
      FValue := Value;
    FEmptyValue := Value;
  end;
end;

// Polaris begin

procedure TJvLookupControl.SetFieldValue(Field: TField; const Value: string);
begin
  if Value = FEmptyValue then
    if (FEmptyValue = EmptyStr) and FEmptyStrIsNull then
      Field.Clear
    else
      Field.AsString := FEmptyValue
  else
    Field.AsString := Value;
end;

procedure TJvLookupControl.SetEmptyStrIsNull(const Value: Boolean);
begin
  if FEmptyStrIsNull <> Value then
  begin
    FEmptyStrIsNull := Value;
    if CanModify and (FDataLink.DataSource <> nil) and FDataLink.Edit then
      if FMasterField <> nil then
        SetFieldValue(FMasterField, FValue)
      else
        SetFieldValue(FDataField, FValue);
  end;
end;
// Polaris end

procedure TJvLookupControl.SetEmptyItemColor(Value: TColor);
begin
  if FEmptyItemColor <> Value then
  begin
    FEmptyItemColor := Value;
    if not (csReading in ComponentState) and (DisplayEmpty <> '') then
      Invalidate;
  end;
end;

procedure TJvLookupControl.UpdateDisplayEmpty(const Value: string);
begin
end;

procedure TJvLookupControl.SetDisplayValue(const Value: string);
{var S: string; }// Polaris
begin
  if (FDisplayValue <> Value) and CanModify and (FDataLink.DataSource <> nil) and
    Locate(FDisplayField, Value, True) then
  begin
    // S := FValue;  // Polaris
    if FDataLink.Edit then
    begin
      // if FMasterField <> nil then FMasterField.AsString := S
      //   else FDataField.AsString := S;
      if FMasterField <> nil then
        SetFieldValue(FMasterField, FValue) // Polaris
      else
        SetFieldValue(FDataField, FValue); // Polaris
    end;
  end
  else
  if FDisplayValue <> Value then
  begin
    FDisplayValue := Value;
    DisplayValueChanged;
    Change;
  end;
end;

procedure TJvLookupControl.UpdateKeyValue;
begin
  if FMasterField <> nil then
    FValue := FMasterField.AsString
  else
    FValue := FEmptyValue;
  KeyValueChanged;
end;

procedure TJvLookupControl.SetValueKey(const Value: string);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    KeyValueChanged;
  end;
end;

procedure TJvLookupControl.SetValue(const Value: string);
begin
  if Value <> FValue then
  begin // Polaris // begin added
    if CanModify and (FDataLink.DataSource <> nil) and FDataLink.Edit then
    begin
      // if FMasterField <> nil then FMasterField.AsString := Value
      //   else FDataField.AsString := Value;
      if FMasterField <> nil then
        SetFieldValue(FMasterField, Value) // Polaris
      else
        SetFieldValue(FDataField, Value); // Polaris
    end
    else // begin  // Polaris
      SetValueKey(Value);
    Change;
  end;
end;

procedure TJvLookupControl.SetLookupDisplay(const Value: string);
begin
  if FLookupDisplay <> Value then
  begin
    FLookupDisplay := Value;
    ListLinkActiveChanged;
    if FListActive then
      DataLinkRecordChanged(nil);
  end;
end;

procedure TJvLookupControl.SetLookupSource(Value: TDataSource);
begin
  {$IFDEF WIN32}
  CheckNotFixed;
  {$ENDIF}
  FLookupLink.DataSource := Value;
  {$IFDEF WIN32}
  if Value <> nil then
    Value.FreeNotification(Self);
  {$ENDIF}
  if Value <> nil then
    FLocate.DataSet := Value.DataSet
  else
    FLocate.DataSet := nil;
  if FListActive then
    DataLinkRecordChanged(nil);
end;

procedure TJvLookupControl.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TJvLookupControl.GetItemHeight: Integer;
begin
  Result := {Max(GetTextHeight, FItemHeight);} GetTextHeight;
end;

procedure TJvLookupControl.SetItemHeight(Value: Integer);
begin
  if not (csReading in ComponentState) then
    FItemHeight := Max(DefaultTextHeight, Value)
  else
    FItemHeight := Value;
  Perform(CM_FONTCHANGED, 0, 0);
end;

function TJvLookupControl.ItemHeightStored: Boolean;
begin
  Result := FItemHeight > DefaultTextHeight;
end;

procedure TJvLookupControl.DrawPicture(Canvas: TCanvas; Rect: TRect;
  Image: TGraphic);
var
  X, Y, SaveIndex: Integer;
  {$IFDEF WIN32}
  Ico: HICON;
  W, H: Integer;
  {$ENDIF}
begin
  if Image <> nil then
  begin
    X := (Rect.Right + Rect.Left - Image.Width) div 2;
    Y := (Rect.Top + Rect.Bottom - Image.Height) div 2;
    SaveIndex := SaveDC(Canvas.Handle);
    try
      IntersectClipRect(Canvas.Handle, Rect.Left, Rect.Top, Rect.Right,
        Rect.Bottom);
      if Image is TBitmap then
        DrawBitmapTransparent(Canvas, X, Y, TBitmap(Image),
          TBitmap(Image).TransparentColor)
      {$IFDEF WIN32}
      else
      if Image is TIcon then
      begin
        Ico := CreateRealSizeIcon(TIcon(Image));
        try
          GetIconSize(Ico, W, H);
          DrawIconEx(Canvas.Handle, (Rect.Right + Rect.Left - W) div 2,
            (Rect.Top + Rect.Bottom - H) div 2, Ico, W, H, 0, 0, DI_NORMAL);
        finally
          DestroyIcon(Ico);
        end;
      end
      {$ENDIF}
      else
        Canvas.Draw(X, Y, Image);
    finally
      RestoreDC(Canvas.Handle, SaveIndex);
    end;
  end;
end;

function TJvLookupControl.GetPicture(Current, Empty: Boolean;
  var TextMargin: Integer): TGraphic;
begin
  TextMargin := 0;
  Result := nil;
  if Assigned(FOnGetImage) then
    FOnGetImage(Self, Empty, Result, TextMargin);
end;

procedure TJvLookupControl.WMGetDlgCode(var Msg: TMessage);
begin
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TJvLookupControl.WMKillFocus(var Msg: TMessage);
begin
  FFocused := False;
  inherited;
  Invalidate;
end;

procedure TJvLookupControl.WMSetFocus(var Msg: TMessage);
begin
  FFocused := True;
  inherited;
  Invalidate;
end;

function TJvLookupControl.Locate(const SearchField: TField;
  const AValue: string; Exact: Boolean): Boolean;
begin
  FLocate.IndexSwitch := FIndexSwitch;
  Result := False;
  try
    if not ValueIsEmpty(AValue) and (SearchField <> nil) then
    begin
      Result := FLocate.Locate(SearchField.FieldName, AValue, Exact, not IgnoreCase);
      if Result then
      begin
        if SearchField = FDisplayField then
          FValue := FKeyField.AsString;
        UpdateDisplayValue;
      end;
    end;
  except
  end;
end;

function TJvLookupControl.EmptyRowVisible: Boolean;
begin
  Result := DisplayEmpty <> EmptyStr;
end;

procedure TJvLookupControl.UpdateDisplayValue;
begin
  if not ValueIsEmpty(FValue) then
  begin
    if FDisplayField <> nil then
      FDisplayValue := FDisplayField.AsString
    else
      FDisplayValue := '';
  end
  else
    FDisplayValue := '';
end;

function TJvLookupControl.GetWindowWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FListFields.Count - 1 do
    Inc(Result, TField(FListFields[I]).DisplayWidth);
  Canvas.Font := Font;
  Result := Min(Result * Canvas.TextWidth('M') + FListFields.Count * 4 +
    GetSystemMetrics(SM_CXVSCROLL), Screen.Width);
end;

procedure TJvLookupControl.SetLookupFormat(const Value: string);
begin
  if Value <> FLookupFormat then
  begin
    CheckLookupFormat(Value);
    FLookupFormat := Value;
    ListLinkActiveChanged;
    if FListActive then
      DataLinkRecordChanged(nil);
  end;
end;

function TJvLookupControl.DoFormatLine: string;
var
  J, LastFieldIndex: Integer;
  Field: TField;
  LStringList: array of string;
  LVarList: array of TVarRec;
begin
  Result := '';
  LastFieldIndex := FListFields.Count - 1;
  if LookupFormat > '' then
  begin
    SetLength(LStringList, LastFieldIndex + 1);
    SetLength(LVarList, LastFieldIndex + 1);

    for J := 0 to LastFieldIndex do
    begin
      LStringList[J] := TField(FListFields[J]).DisplayText;
      LVarList[J].VPChar := PChar(LStringList[J]);
      LVarList[J].VType := vtPChar;
    end;
    Result := Format(LookupFormat, LVarList);
  end
  else
    for J := 0 to LastFieldIndex do
    begin
      Field := FListFields[J];
      Result := Result + Field.DisplayText;
      if J < LastFieldIndex then
        Result := Result + FFieldsDelimiter + ' ';
    end;
end;

//=== TJvDBLookupList ========================================================

constructor TJvDBLookupList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 121;
  Ctl3D := True;
  FBorderStyle := bsSingle;
  {$IFDEF WIN32}
  ControlStyle := [csOpaque, csDoubleClicks];
  {$ELSE}
  ControlStyle := [csFramed, csOpaque, csDoubleClicks];
  {$ENDIF}
  RowCount := 7;
end;

procedure TJvDBLookupList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_VSCROLL;
    if FBorderStyle = bsSingle then
      {$IFDEF WIN32}
      if NewStyleControls and Ctl3D then
        ExStyle := ExStyle or WS_EX_CLIENTEDGE
      else
        Style := Style or WS_BORDER;
      {$ELSE}
      Style := Style or WS_BORDER;
      {$ENDIF}
  end;
end;

procedure TJvDBLookupList.CreateWnd;
begin
  inherited CreateWnd;
  UpdateScrollBar;
end;

procedure TJvDBLookupList.Loaded;
begin
  inherited Loaded;
  Height := Height;
end;

function TJvDBLookupList.GetKeyIndex: Integer;
var
  FieldValue: string;
begin
  if not ValueIsEmpty(FValue) then
    for Result := 0 to FRecordCount - 1 do
    begin
      FLookupLink.ActiveRecord := Result;
      FieldValue := FKeyField.AsString;
      FLookupLink.ActiveRecord := FRecordIndex;
      if FieldValue = FValue then
        Exit;
    end;
  Result := -1;
end;

procedure TJvDBLookupList.KeyDown(var Key: Word; Shift: TShiftState);
var
  Delta, KeyIndex, EmptyRow: Integer;
begin
  inherited KeyDown(Key, Shift);
  FSelectEmpty := False;
  EmptyRow := Ord(EmptyRowVisible);
  if CanModify then
  begin
    Delta := 0;
    case Key of
      VK_UP, VK_LEFT:
        Delta := -1;
      VK_DOWN, VK_RIGHT:
        Delta := 1;
      VK_PRIOR:
        Delta := 1 - (FRowCount - EmptyRow);
      VK_NEXT:
        Delta := (FRowCount - EmptyRow) - 1;
      VK_HOME:
        Delta := -MaxInt;
      VK_END:
        Delta := MaxInt;
    end;
    if Delta <> 0 then
    begin
      if ValueIsEmpty(Value) and (EmptyRow > 0) and (Delta < 0) then
        FSelectEmpty := True;
      FSearchText := '';
      if Delta = -MaxInt then
        FLookupLink.DataSet.First
      else
      if Delta = MaxInt then
        FLookupLink.DataSet.Last
      else
      begin
        KeyIndex := GetKeyIndex;
        if KeyIndex >= 0 then
        begin
          FLookupLink.DataSet.MoveBy(KeyIndex - FRecordIndex);
        end
        else
        begin
          KeyValueChanged;
          Delta := 0;
        end;
        FLookupLink.DataSet.MoveBy(Delta);
        if FLookupLink.DataSet.Bof and (Delta < 0) and (EmptyRow > 0) then
          FSelectEmpty := True;
      end;
      SelectCurrent;
    end;
  end;
end;

procedure TJvDBLookupList.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  ProcessSearchKey(Key);
end;

procedure TJvDBLookupList.KeyValueChanged;
begin
  if FListActive and not FLockPosition then
    if not LocateKey then
      FLookupLink.DataSet.First;
end;

procedure TJvDBLookupList.DisplayValueChanged;
begin
  if FListActive and not FLockPosition then
    if not LocateDisplay then
      FLookupLink.DataSet.First;
end;

procedure TJvDBLookupList.ListLinkActiveChanged;
begin
  try
    inherited ListLinkActiveChanged;
  finally
    if FListActive and not FLockPosition then
    begin
      if Assigned(FMasterField) then
        UpdateKeyValue
      else
        KeyValueChanged;
    end
    else
      ListDataChanged;
  end;
end;

procedure TJvDBLookupList.ListDataChanged;
begin
  if FListActive then
  begin
    FRecordIndex := FLookupLink.ActiveRecord;
    FRecordCount := FLookupLink.RecordCount;
    FKeySelected := not ValueIsEmpty(FValue) or not FLookupLink.DataSet.Bof;
  end
  else
  begin
    FRecordIndex := 0;
    FRecordCount := 0;
    FKeySelected := False;
  end;
  if HandleAllocated then
  begin
    UpdateScrollBar;
    Invalidate;
  end;
end;

procedure TJvDBLookupList.ListLinkDataChanged;
begin
  ListDataChanged;
end;

procedure TJvDBLookupList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FSearchText := '';
    if not FPopup then
    begin
      if CanFocus then
        SetFocus;
      if not FFocused then
        Exit;
    end;
    if CanModify then
      if ssDouble in Shift then
      begin
        if FRecordIndex = Y div GetTextHeight then
          DblClick;
      end
      else
      begin
        MouseCapture := True;
        FTracking := True;
        SelectItemAt(X, Y);
      end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvDBLookupList.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTracking then
  begin
    SelectItemAt(X, Y);
    FMousePos := Y;
    TimerScroll;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvDBLookupList.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FTracking then
  begin
    StopTracking;
    SelectItemAt(X, Y);
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvDBLookupList.DrawItemText(Canvas: TCanvas; Rect: TRect;
  Selected, IsEmpty: Boolean);
var
  J, W, X, ATop, TextWidth, LastFieldIndex: Integer;
  S: string;
  Field: TField;
  R: TRect;
  AAlignment: TAlignment;
begin
  TextWidth := Canvas.TextWidth('M');
  LastFieldIndex := FListFields.Count - 1;
  R := Rect;
  R.Right := R.Left;
  S := '';
  ATop := (R.Bottom + R.Top - Canvas.TextHeight('Xy')) div 2;
  if FListStyle = lsFixed then
    for J := 0 to LastFieldIndex do
    begin
      Field := FListFields[J];
      if J < LastFieldIndex then
        W := Field.DisplayWidth * TextWidth + 4
      else
        W := ClientWidth - R.Right;
      if IsEmpty then
      begin
        if J = 0 then
        begin
          S := DisplayEmpty;
        end
        else
          S := '';
      end
      else
        S := Field.DisplayText;
      X := 2;
      AAlignment := Field.Alignment;
      {$IFDEF COMPILER4_UP}
      if UseRightToLeftAlignment then
        ChangeBiDiModeAlignment(AAlignment);
      {$ENDIF}
      case AAlignment of
        taRightJustify:
          X := W - Canvas.TextWidth(S) - 3;
        taCenter:
          X := (W - Canvas.TextWidth(S)) div 2;
      end;
      R.Left := R.Right;
      R.Right := R.Right + W;
      {$IFDEF COMPILER4_UP}
      if SysLocale.MiddleEast and UseRightToLeftReading then
        Canvas.TextFlags := Canvas.TextFlags or ETO_RTLREADING
      else
        Canvas.TextFlags := Canvas.TextFlags and not ETO_RTLREADING;
      {$ENDIF}
      Canvas.TextRect(R, R.Left + X, ATop, S);
      if J < LastFieldIndex then
      begin
        Canvas.MoveTo(R.Right, R.Top);
        Canvas.LineTo(R.Right, R.Bottom);
        Inc(R.Right);
        if R.Right >= ClientWidth then
          Break;
      end;
    end
  else
  if not IsEmpty then
    S := DoFormatLine;
  if FListStyle = lsDelimited then
  begin
    if IsEmpty then
      S := DisplayEmpty;
    R.Left := Rect.Left;
    R.Right := Rect.Right;
    {$IFDEF COMPILER4_UP}
    if SysLocale.MiddleEast and UseRightToLeftReading then
      Canvas.TextFlags := Canvas.TextFlags or ETO_RTLREADING
    else
      Canvas.TextFlags := Canvas.TextFlags and not ETO_RTLREADING;
    {$ENDIF}
    Canvas.TextRect(R, R.Left + 2, ATop, S);
  end;
end;

procedure TJvDBLookupList.Paint;
var
  I, J, TextHeight, TextMargin: Integer;
  Image: TGraphic;
  Bmp: TBitmap;
  R, ImageRect: TRect;
  Selected: Boolean;
begin
  Bmp := TBitmap.Create;
  try
    Canvas.Font := Font;
    TextHeight := GetTextHeight;
    if ColorToRGB(Color) <> ColorToRGB(clBtnFace) then
      Canvas.Pen.Color := clBtnFace
    else
      Canvas.Pen.Color := clBtnShadow;
    for I := 0 to FRowCount - 1 do
    begin
      J := I - Ord(EmptyRowVisible);
      Canvas.Font.Color := Font.Color;
      Canvas.Brush.Color := Color;
      Selected := not FKeySelected and (I = 0) and not EmptyRowVisible;
      R.Top := I * TextHeight;
      R.Bottom := R.Top + TextHeight;
      if I < FRecordCount + Ord(EmptyRowVisible) then
      begin
        if (I = 0) and (J = -1) then
        begin
          if ValueIsEmpty(FValue) then
          begin
            Canvas.Font.Color := clHighlightText;
            Canvas.Brush.Color := clHighlight;
            Selected := True;
          end
          else
            Canvas.Brush.Color := EmptyItemColor;
          R.Left := 0;
          R.Right := ClientWidth;
          Image := GetPicture(False, True, TextMargin);
          if TextMargin > 0 then
          begin
            with Bmp do
            begin
              Canvas.Font := Self.Canvas.Font;
              Canvas.Brush := Self.Canvas.Brush;
              Canvas.Pen := Self.Canvas.Pen;
              Width := WidthOf(R);
              Height := HeightOf(R);
            end;
            ImageRect := Bounds(0, 0, TextMargin, HeightOf(R));
            Bmp.Canvas.FillRect(ImageRect);
            if Image <> nil then
              DrawPicture(Bmp.Canvas, ImageRect, Image);
            DrawItemText(Bmp.Canvas, Bounds(TextMargin, 0, WidthOf(R) - TextMargin,
              HeightOf(R)), Selected, True);
            Canvas.Draw(R.Left, R.Top, Bmp);
          end
          else
            DrawItemText(Canvas, R, Selected, True);
        end
        else
        begin
          FLookupLink.ActiveRecord := J;
          if not ValueIsEmpty(FValue) and (FKeyField.AsString = FValue) then
          begin
            Canvas.Font.Color := clHighlightText;
            Canvas.Brush.Color := clHighlight;
            Selected := True;
          end;
          R.Left := 0;
          R.Right := ClientWidth;
          Image := GetPicture(False, False, TextMargin);
          if TextMargin > 0 then
          begin
            with Bmp do
            begin
              Canvas.Font := Self.Canvas.Font;
              Canvas.Brush := Self.Canvas.Brush;
              Canvas.Pen := Self.Canvas.Pen;
              Width := WidthOf(R);
              Height := HeightOf(R);
            end;
            ImageRect := Bounds(0, 0, TextMargin, HeightOf(R));
            Bmp.Canvas.FillRect(ImageRect);
            if Image <> nil then
              DrawPicture(Bmp.Canvas, ImageRect, Image);
            DrawItemText(Bmp.Canvas, Bounds(TextMargin, 0, WidthOf(R) - TextMargin,
              HeightOf(R)), Selected, False);
            Canvas.Draw(R.Left, R.Top, Bmp);
          end
          else
            DrawItemText(Canvas, R, Selected, False);
        end;
      end;
      R.Left := 0;
      R.Right := ClientWidth;
      if J >= FRecordCount then
        Canvas.FillRect(R);
      if Selected and (FFocused or FPopup) then
        Canvas.DrawFocusRect(R);
    end;
  finally
    Bmp.Free;
  end;
  if FRecordCount <> 0 then
    FLookupLink.ActiveRecord := FRecordIndex;
end;

procedure TJvDBLookupList.SelectCurrent;
begin
  FLockPosition := True;
  try
    if FSelectEmpty then
      ResetField
    else
      SelectKeyValue(FKeyField.AsString);
  finally
    FSelectEmpty := False;
    FLockPosition := False;
  end;
end;

procedure TJvDBLookupList.SelectItemAt(X, Y: Integer);
var
  Delta: Integer;
begin
  if Y < 0 then
    Y := 0;
  if Y >= ClientHeight then
    Y := ClientHeight - 1;
  Delta := Y div GetTextHeight;
  if (Delta = 0) and EmptyRowVisible then
    FSelectEmpty := True
  else
  begin
    Delta := Delta - FRecordIndex;
    if EmptyRowVisible then
      Dec(Delta);
    FLookupLink.DataSet.MoveBy(Delta);
  end;
  SelectCurrent;
end;

procedure TJvDBLookupList.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
    if not (csReading in ComponentState) then
    begin
      Height := Height;
      RowCount := RowCount;
    end;
  end;
end;

procedure TJvDBLookupList.UpdateDisplayEmpty(const Value: string);
begin
  UpdateBufferCount(RowCount - Ord(Value <> EmptyStr));
end;

procedure TJvDBLookupList.UpdateBufferCount(Rows: Integer);
begin
  if FLookupLink.BufferCount <> Rows then
  begin
    FLookupLink.BufferCount := Rows;
    ListLinkDataChanged;
  end;
end;

procedure TJvDBLookupList.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  BorderSize, TextHeight, Rows: Integer;
begin
  BorderSize := GetBorderSize;
  TextHeight := GetTextHeight;
  Rows := (AHeight - BorderSize) div TextHeight;
  if Rows < 1 then
    Rows := 1;
  FRowCount := Rows;
  UpdateBufferCount(Rows - Ord(EmptyRowVisible));
  if not (csReading in ComponentState) then
    AHeight := Rows * TextHeight + BorderSize;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TJvDBLookupList.SetRowCount(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value > 50 then
    Value := 50;
  Height := Value * GetTextHeight + GetBorderSize;
end;

procedure TJvDBLookupList.StopTimer;
begin
  if FTimerActive then
  begin
    // (rom) why not a TTimer?
    KillTimer(Handle, 1);
    FTimerActive := False;
  end;
end;

procedure TJvDBLookupList.StopTracking;
begin
  if FTracking then
  begin
    StopTimer;
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TJvDBLookupList.TimerScroll;
var
  Delta, Distance, Interval: Integer;
begin
  Delta := 0;
  Distance := 0;
  if FMousePos < 0 then
  begin
    Delta := -1;
    Distance := -FMousePos;
  end;
  if FMousePos >= ClientHeight then
  begin
    Delta := 1;
    Distance := FMousePos - ClientHeight + 1;
  end;
  if Delta = 0 then
    StopTimer
  else
  begin
    if FLookupLink.DataSet.MoveBy(Delta) <> 0 then
      SelectCurrent;
    Interval := 200 - Distance * 15;
    if Interval < 0 then
      Interval := 0;
    SetTimer(Handle, 1, Interval, nil);
    FTimerActive := True;
  end;
end;

procedure TJvDBLookupList.UpdateScrollBar;
(*
{$IFDEF COMPILER3_UP}
var
  SIOld, SINew: TScrollInfo;
begin
  if FLookuplink.Active and HandleAllocated then begin
    with FLookuplink.DataSet do begin
      SIOld.cbSize := sizeof(SIOld);
      SIOld.fMask := SIF_ALL;
      GetScrollInfo(Self.Handle, SB_VERT, SIOld);
      SINew := SIOld;
      if IsSequenced then begin
        SINew.nMin := 1;
        SINew.nPage := Self.FRowCount - Ord(EmptyRowVisible);
        SINew.nMax := RecordCount + SINew.nPage - 1;
        if State in [dsInactive, dsBrowse, dsEdit] then
          SINew.nPos := RecNo;
      end
      else begin
        SINew.nMin := 0;
        SINew.nPage := 0;
        if Self.FRecordCount = (FRowCount - Ord(EmptyRowVisible)) then begin
          SINew.nMax := 4;
          if Bof then SINew.nPos := 0
          else if Eof then SINew.nPos := 4
          else SINew.nPos := 2;
        end
        else begin
          SINew.nMax := 0;
          SINew.nPos := 0;
        end;
      end;
      if (SINew.nMin <> SIOld.nMin) or (SINew.nMax <> SIOld.nMax) or
        (SINew.nPage <> SIOld.nPage) or (SINew.nPos <> SIOld.nPos) then
        SetScrollInfo(Self.Handle, SB_VERT, SINew, True);
    end;
  end
  else begin
    SetScrollRange(Handle, SB_VERT, 0, 0, False);
    SetScrollPos(Handle, SB_VERT, 0, True);
  end;
end;
{$ELSE}
*)
var
  Pos, Max: Integer;
  CurPos, MaxPos: Integer;
begin
  if FLookupLink.Active then
  begin
    Pos := 0;
    Max := 0;
    if FRecordCount = (FRowCount - Ord(EmptyRowVisible)) then
    begin
      Max := 4;
      if not FLookupLink.DataSet.Bof then
        if not FLookupLink.DataSet.Eof then
          Pos := 2
        else
          Pos := 4;
    end;
    GetScrollRange(Handle, SB_VERT, CurPos, MaxPos);
    if MaxPos = 0 then
      MaxPos := FRecordCount;
    CurPos := GetScrollPos(Handle, SB_VERT);
    if Max <> MaxPos then
      SetScrollRange(Handle, SB_VERT, 0, Max, False);
    if CurPos <> Pos then
      SetScrollPos(Handle, SB_VERT, Pos, True);
  end
  else
  begin
    SetScrollRange(Handle, SB_VERT, 0, 0, False);
    SetScrollPos(Handle, SB_VERT, 0, True);
  end;
end;

procedure TJvDBLookupList.CMCtl3DChanged(var Msg: TMessage);
begin
  {$IFDEF WIN32}
  if NewStyleControls and (FBorderStyle = bsSingle) then
  begin
    RecreateWnd;
    if not (csReading in ComponentState) then
      RowCount := RowCount;
  end;
  inherited;
  {$ELSE}
  inherited;
  Invalidate;
  if not (csReading in ComponentState) then
    RowCount := RowCount;
  {$ENDIF}
end;

procedure TJvDBLookupList.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  if not (csReading in ComponentState) then
    Height := Height;
end;

procedure TJvDBLookupList.WMCancelMode(var Msg: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TJvDBLookupList.WMTimer(var Msg: TMessage);
begin
  TimerScroll;
end;

procedure TJvDBLookupList.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  if csDesigning in ComponentState then
  begin
    if FLookupLink.Active then
      DefaultHandler(Msg)
    else
      inherited;
  end
  else
    inherited;
end;

function TJvDBLookupList.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
  begin
    if FLookupLink.DataSet = nil then
      Exit;

    with FLookupLink.DataSet do
      { FRecordCount - FRecordIndex - 1  = #records till end of visible list
        FRecordCount div 2               = half visible list.
      }
      if Shift * [ssShift, ssCtrl] <> [] then
        { 1 line down }
        Result := MoveBy(FRecordCount - FRecordIndex) <> 0
      else
        { Half Page down }
        Result := MoveBy(FRecordCount - FRecordIndex + FRecordCount div 2 - 1) <> 0;
  end;
end;

function TJvDBLookupList.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then
  begin
    if FLookupLink.DataSet = nil then
      Exit;

    with FLookupLink.DataSet do
      { -FRecordIndex        = #records till begin of visible list
        FRecordCount div 2   = half visible list.
      }
      if Shift * [ssShift, ssCtrl] <> [] then
        { One line up }
        Result := MoveBy(-FRecordIndex - 1) <> 0
      else
        { Half Page up }
        Result := MoveBy(-FRecordIndex - FRecordCount div 2) <> 0;
  end;
end;

procedure TJvDBLookupList.WMVScroll(var Msg: TWMVScroll);
begin
  FSearchText := '';
  if FLookupLink.DataSet = nil then
    Exit;

  with Msg, FLookupLink.DataSet do
    case ScrollCode of
      SB_LINEUP:
        MoveBy(-FRecordIndex - 1);
      SB_LINEDOWN:
        MoveBy(FRecordCount - FRecordIndex);
      SB_PAGEUP:
        MoveBy(-FRecordIndex - FRecordCount + 1);
      SB_PAGEDOWN:
        MoveBy(FRecordCount - FRecordIndex + FRecordCount - 2);
      SB_THUMBPOSITION:
        begin
          case Pos of
            0:
              First;
            1:
              MoveBy(-FRecordIndex - FRecordCount + 1);
            2:
              Exit;
            3:
              MoveBy(FRecordCount - FRecordIndex + FRecordCount - 2);
            4:
              Last;
          end;
        end;
      SB_BOTTOM:
        Last;
      SB_TOP:
        First;
    end;
end;

//=== TJvPopupDataList =======================================================

constructor TJvPopupDataList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TJvLookupControl then
    FCombo := TJvLookupControl(AOwner);
  {$IFDEF WIN32}
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
  {$ELSE}
  ControlStyle := [csOpaque];
  {$ENDIF}
  FPopup := True;
  TabStop := False;
  ParentCtl3D := False;
  Ctl3D := False;
end;

procedure TJvPopupDataList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER;
    {$IFDEF WIN32}
    ExStyle := WS_EX_TOOLWINDOW;
    {$ENDIF}
    {$IFDEF COMPILER4_UP}
    AddBiDiModeExStyle(ExStyle);
    {$ENDIF}
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

{$IFNDEF WIN32}
procedure TJvPopupDataList.CreateWnd;
begin
  inherited CreateWnd;
  if csDesigning in ComponentState then
    SetParent(nil);
end;
{$ENDIF}

procedure TJvPopupDataList.WMMouseActivate(var Msg: TMessage);
begin
  Msg.Result := MA_NOACTIVATE;
end;

procedure TJvPopupDataList.Click;
begin
  inherited Click;
  if Assigned(FCombo) and TJvDBLookupCombo(FCombo).FListVisible then
    TJvDBLookupCombo(FCombo).InvalidateText;
end;

procedure TJvPopupDataList.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if Assigned(FCombo) and TJvDBLookupCombo(FCombo).FListVisible then
    TJvDBLookupCombo(FCombo).InvalidateText;
end;

//=== TJvDBLookupCombo =======================================================

constructor TJvDBLookupCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF WIN32}
  ControlStyle := ControlStyle + [csReplicatable] - [csSetCaption];
  {$ELSE}
  ControlStyle := [csFramed, csOpaque];
  {$ENDIF}
  Width := 145;
  Height := 0;
  FDataList := TJvPopupDataList.Create(Self);
  FDataList.Visible := False;
  FDataList.Parent := Self;
  FDataList.OnMouseUp := ListMouseUp;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FDropDownCount := 8;
  FDisplayValues := TStringList.Create;
  FSelImage := TPicture.Create;
  {$IFNDEF WIN32}
  FBtnGlyph := TBitmap.Create;
  { Load ComboBox button glyph }
  // (rom) needs improvement
  FBtnGlyph.Handle := LoadBitmap(0, PChar(32738));
  FBtnDisabled := CreateDisabledBitmap(FBtnGlyph, clBlack);
  {$ENDIF}
  Height := {GetMinHeight} 21;
  FIgnoreCase := True;
  FEscapeClear := True;
end;

destructor TJvDBLookupCombo.Destroy;
begin
  {$IFNDEF WIN32}
  FBtnDisabled.Free;
  FBtnGlyph.Free;
  {$ENDIF}
  FSelImage.Free;
  FSelImage := nil;
  FDisplayValues.Free;
  FDisplayValues := nil;
  inherited Destroy;
end;

procedure TJvDBLookupCombo.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    {$IFDEF WIN32}
    if NewStyleControls and Ctl3D then
      ExStyle := ExStyle or WS_EX_CLIENTEDGE
    else
      Style := Style or WS_BORDER;
  {$ELSE}
    Style := Style or WS_BORDER;
  {$ENDIF}
end;

procedure TJvDBLookupCombo.CloseUp(Accept: Boolean);
var
  ListValue: string;
begin
  if FListVisible then
  begin
    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    ListValue := FDataList.Value;
    SetWindowPos(FDataList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
    FDataList.LookupSource := nil;
    Invalidate;
    FSearchText := '';
    FDataList.FSearchText := '';
    if Accept and CanModify and (Value <> ListValue) then
      SelectKeyValue(ListValue);
    if CanFocus then
      SetFocus;
    if Assigned(FOnCloseUp) then
      FOnCloseUp(Self);
  end;
end;

function TJvDBLookupCombo.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
  begin
    if FLookupLink.DataSet = nil then
      Exit;

    { Simulate up or down key, see code in KeyDown }
    if FListActive then
      if ssAlt in Shift then
      begin
        if FListVisible then
          CloseUp(True)
        else
          DropDown;
        Result := True;
      end
      else
      if not FListVisible and not ReadOnly then
      begin
        if not LocateKey then
          FLookupLink.DataSet.First
        else
          FLookupLink.DataSet.MoveBy(1);
        SelectKeyValue(FKeyField.AsString);
        Result := True;
      end;
    if not Result and FListVisible then
      Result := FDataList.DoMouseWheelDown(Shift, MousePos);
  end;
end;

function TJvDBLookupCombo.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then
  begin
    if FLookupLink.DataSet = nil then
      Exit;

    { Simulate up or down key, see code in KeyDown }
    if FListActive then
      if ssAlt in Shift then
      begin
        if FListVisible then
          CloseUp(True)
        else
          DropDown;
        Result := True;
      end
      else
      if not FListVisible and not ReadOnly then
      begin
        if not LocateKey then
          FLookupLink.DataSet.First
        else
          FLookupLink.DataSet.MoveBy(-1);
        SelectKeyValue(FKeyField.AsString);
        Result := True;
      end;
    if not Result and FListVisible then
      Result := FDataList.DoMouseWheelUp(Shift, MousePos);
  end;
end;

procedure TJvDBLookupCombo.DropDown;
var
  P: TPoint;
  I, Y: Integer;
  S: string;
  {$IFDEF COMPILER6_UP}
  Animate: BOOL;
  SlideStyle: Integer;
  {$ENDIF}
begin
  if not FListVisible and {FListActive} CanModify then
  begin
    if Assigned(FOnDropDown) then
      FOnDropDown(Self);
    FDataList.Color := Color;
    FDataList.Font := Font;
    FDataList.ItemHeight := ItemHeight;
    FDataList.ReadOnly := not CanModify;
    FDataList.EmptyValue := EmptyValue;
    FDataList.DisplayEmpty := DisplayEmpty;
    FDataList.EmptyItemColor := EmptyItemColor;
    FDataList.RowCount := DropDownCount;
    FDataList.LookupField := FLookupFieldName;
    FDataList.LookupFormat := FLookupFormat;
    FDataList.ListStyle := FListStyle;
    FDataList.FieldsDelimiter := FFieldsDelimiter;
    FDataList.IgnoreCase := FIgnoreCase;
    FDataList.IndexSwitch := FIndexSwitch;
    FDataList.OnGetImage := OnGetImage;
    // polaris    if FDisplayField <> nil then FAlignment := FDisplayField.Alignment;
    S := '';
    for I := 0 to FListFields.Count - 1 do
      S := S + TField(FListFields[I]).FieldName + ';';
    FDataList.LookupDisplay := S;
    FDataList.LookupDisplayIndex := FListFields.IndexOf(FDisplayField);
    {FDataList.FLockPosition := True;}
    try
      FDataList.LookupSource := FLookupLink.DataSource;
    finally
      {FDataList.FLockPosition := False;}
    end;
    FDataList.SetValueKey(Value);
    {FDataList.KeyValueChanged;}
    if FDropDownWidth > 0 then
      FDataList.Width := FDropDownWidth
    else
    if FDropDownWidth < 0 then
      FDataList.Width := Max(Width, FDataList.GetWindowWidth)
    else
      FDataList.Width := Width;
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FDataList.Height > Screen.Height then
      Y := P.Y - FDataList.Height;
    case FDropDownAlign of
      daRight:
        Dec(P.X, FDataList.Width - Width);
      daCenter:
        Dec(P.X, (FDataList.Width - Width) div 2);
    end;
    if P.X + FDataList.Width > Screen.Width then
      P.X := Screen.Width - FDataList.Width;

    {$IFDEF COMPILER6_UP}
    { Use slide-open effect for combo boxes if wanted. This is also possible
      for D5<, but D5< does not define AnimateWindowProc in Controls.pas. See
      TJvBalloonHint.pas to solve this }
    SystemParametersInfo(SPI_GETCOMBOBOXANIMATION, 0, @Animate, 0);
    if Assigned(AnimateWindowProc) and Animate then
    begin
      SetWindowPos(FDataList.Handle, HWND_TOP, Max(P.X, 0), Y, 0, 0,
        SWP_NOSIZE or SWP_NOACTIVATE {or SWP_SHOWWINDOW});
      if Y < P.Y then
        SlideStyle := AW_VER_NEGATIVE
      else
        SlideStyle := AW_VER_POSITIVE;
      AnimateWindowProc(FDataList.Handle, 150, SlideStyle or AW_SLIDE);
      ShowWindow(FDataList.Handle, SW_SHOWNOACTIVATE);
      FDataList.Invalidate;
    end
    else
    {$ENDIF COMPILER6_UP}
      SetWindowPos(FDataList.Handle, HWND_TOP, Max(P.X, 0), Y, 0, 0,
        SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);

    FListVisible := True;
    InvalidateText;
    Repaint;
  end;
end;

function TJvDBLookupCombo.GetMinHeight: Integer;
begin
  Result := DefaultTextHeight + GetBorderSize + 3;
end;

procedure TJvDBLookupCombo.UpdateFieldText;
var
  I: Integer;
  S: string;
begin
  if FDisplayValues <> nil then
    FDisplayValues.Clear;
  if DisplayAllFields then
  begin
    S := DoFormatLine;
    if (ListStyle = lsFixed) and Assigned(FDisplayValues) then
      for I := 0 to FListFields.Count - 1 do
        //begin
          //if S <> '' then
          //  S := S + FFieldsDelimiter + ' ';
          //S := S + TField(FListFields[I]).DisplayText;
        //  begin
        with TField(FListFields[I]) do
          FDisplayValues.AddObject(DisplayText,
            TObject(MakeLong(DisplayWidth, Ord(Alignment))));
    //  end;
    //end;
    if S = '' then
      S := FDisplayField.DisplayText;
    inherited Text := S;
  end
  else
    inherited Text := FDisplayField.DisplayText;
  FAlignment := FDisplayField.Alignment;
end;

function TJvDBLookupCombo.GetDisplayValues(Index: Integer): string;
begin
  if Assigned(FDisplayValues) and (FDisplayValues.Count > Index) then
    Result := FDisplayValues[Index]
  else
    Result := FDisplayValue;
end;

function TJvDBLookupCombo.GetText: string;
begin
  Result := inherited Text;
end;

procedure TJvDBLookupCombo.InvalidateText;
var
  R: TRect;
begin
  SetRect(R, 1, 1, ClientWidth - FButtonWidth - 1, ClientHeight - 1);
  {$IFNDEF WIN32}
  InflateRect(R, -1, -1);
  {$ENDIF}
  InvalidateRect(Self.Handle, @R, False);
  UpdateWindow(Self.Handle);
end;

procedure TJvDBLookupCombo.KeyDown(var Key: Word; Shift: TShiftState);
var
  Delta: Integer;
begin
  if FListActive and ((Key = VK_UP) or (Key = VK_DOWN)) then
    if ssAlt in Shift then
    begin
      if FListVisible then
        CloseUp(True)
      else
        DropDown;
      Key := 0;
    end
    else
    if not FListVisible and not ReadOnly then
    begin
      if not LocateKey then
        FLookupLink.DataSet.First
      else
      begin
        if Key = VK_UP then
          Delta := -1
        else
          Delta := 1;
        FLookupLink.DataSet.MoveBy(Delta);
      end;
      SelectKeyValue(FKeyField.AsString);
      Key := 0;
    end;
  if (Key <> 0) and FListVisible then
    FDataList.KeyDown(Key, Shift);
  inherited KeyDown(Key, Shift);
end;

procedure TJvDBLookupCombo.KeyPress(var Key: Char);
begin
  if FListVisible then
  begin
    if Key in [#13, #27] then
    begin
      CloseUp(Key = #13);
      Key := #0;
    end
    else
      FDataList.KeyPress(Key)
  end
  else
  begin
    if Key in [#32..#255] then
    begin
      DropDown;
      if FListVisible then
        FDataList.KeyPress(Key);
    end
    else
    if (Key = #27) and FEscapeClear and (not ValueIsEmpty(FValue)) and CanModify then
    begin
      ResetField;
      // Key := #0;
      if FValue = FEmptyValue then
        Key := #0; // Polaris
    end;
  end;
  inherited KeyPress(Key);
  if Key in [#13, #27] then
    GetParentForm(Self).Perform(CM_DIALOGKEY, Byte(Key), 0);
end;

procedure TJvDBLookupCombo.DisplayValueChanged;
begin
  if FListActive and LocateDisplay then
  begin
    FValue := FKeyField.AsString;
    UpdateFieldText;
  end
  else
  begin
    FValue := FEmptyValue;
    inherited Text := DisplayEmpty;
    if FDisplayValues <> nil then
      FDisplayValues.Clear;
    FAlignment := taLeftJustify;
  end;
  UpdateDisplayValue;
  UpdateCurrentImage;
  Invalidate;
end;

procedure TJvDBLookupCombo.KeyValueChanged;
begin
  {$IFDEF WIN32}
  if FLookupMode then
  begin
    if FDisplayValues <> nil then
      FDisplayValues.Clear;
    if FDataLink.Active and (FDataField <> nil) then {begin
      inherited Text := FDataField.DisplayText;
      FAlignment := FDataField.Alignment;
    end}
      if ValueIsEmpty(FValue) then
      begin // Polaris
        inherited Text := DisplayEmpty;
        FAlignment := taLeftJustify;
      end
      else
      begin
        inherited Text := FDataField.DisplayText;
        FAlignment := FDataField.Alignment;
      end
    else
      inherited Text := '';
  end
  else
  {$ENDIF}
  if FListActive and LocateKey then
    UpdateFieldText
  else
  if FListActive then
  begin
    FValue := FEmptyValue;
    inherited Text := DisplayEmpty;
    if FDisplayValues <> nil then
      FDisplayValues.Clear;
    FAlignment := taLeftJustify;
  end
  else
  begin
    if csDesigning in ComponentState then // Polaris
      inherited Text := DisplayEmpty // Polaris
    else // Polaris
      inherited Text := '';
    if FDisplayValues <> nil then
      FDisplayValues.Clear;
  end;
  UpdateDisplayValue;
  UpdateCurrentImage;
  Invalidate;
end;

procedure TJvDBLookupCombo.SetFieldsDelimiter(Value: Char);
begin
  if FFieldsDelimiter <> Value then
  begin
    inherited SetFieldsDelimiter(Value);
    if (ListStyle = lsDelimited) and DisplayAllFields and
      not (csReading in ComponentState) then
      KeyValueChanged;
  end;
end;

procedure TJvDBLookupCombo.SetListStyle(Value: TLookupListStyle);
begin
  if FListStyle <> Value then
  begin
    FListStyle := Value;
    if DisplayAllFields and not (csReading in ComponentState) then
      KeyValueChanged;
  end;
end;

function TJvDBLookupCombo.GetDisplayAllFields: Boolean;
begin
  {$IFDEF WIN32}
  if FLookupMode then
    Result := False
  else
  {$ENDIF}
    Result := FDisplayAllFields;
end;

procedure TJvDBLookupCombo.SetDisplayAllFields(Value: Boolean);
begin
  if FDisplayAllFields <> Value then
  begin
    {$IFDEF WIN32}
    if FLookupMode then
      FDisplayAllFields := False
    else
    {$ENDIF}
      FDisplayAllFields := Value;
    if not (csReading in ComponentState) {$IFDEF WIN32} and not FLookupMode {$ENDIF} then
      KeyValueChanged
    else
      Invalidate;
  end;
end;

procedure TJvDBLookupCombo.ListLinkDataChanged;
begin
  if FDataLink.Active and FDataLink.DataSet.IsLinkedTo(LookupSource) then
    if FListActive then
      DataLinkRecordChanged(nil);
end;

procedure TJvDBLookupCombo.ListLinkActiveChanged;
begin
  inherited ListLinkActiveChanged;
  if FListActive and Assigned(FMasterField) then
    UpdateKeyValue
  else
    KeyValueChanged;
end;

procedure TJvDBLookupCombo.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(FDataList.ClientRect, Point(X, Y)));
end;

procedure TJvDBLookupCombo.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if CanFocus then
      SetFocus;
    if not FFocused then
      Exit;
    if FListVisible then
      CloseUp(False)
    else
    if {FListActive} CanModify then
    begin
      MouseCapture := True;
      FTracking := True;
      TrackButton(X, Y);
      DropDown;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvDBLookupCombo.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then
  begin
    TrackButton(X, Y);
    if FListVisible then
    begin
      ListPos := FDataList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(FDataList.ClientRect, ListPos) then
      begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(FDataList.Handle, WM_LBUTTONDOWN, 0, Longint(MousePos));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvDBLookupCombo.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  StopTracking;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvDBLookupCombo.UpdateCurrentImage;
begin
  FSelImage.Assign(nil);
  FSelMargin := 0;
  FSelImage.Graphic := inherited GetPicture(False, ValueIsEmpty(Value),
    FSelMargin);
end;

function TJvDBLookupCombo.GetPicture(Current, Empty: Boolean;
  var TextMargin: Integer): TGraphic;
begin
  if Current then
  begin
    TextMargin := 0;
    Result := nil;
    if (FSelImage <> nil) and (FSelImage.Graphic <> nil) and
      not FSelImage.Graphic.Empty then
    begin
      Result := FSelImage.Graphic;
      TextMargin := FSelMargin;
    end;
  end
  else
    Result := inherited GetPicture(Current, Empty, TextMargin);
end;

procedure TJvDBLookupCombo.PaintDisplayValues(Canvas: TCanvas; R: TRect;
  ALeft: Integer);
var
  I, LastIndex, TxtWidth: Integer;
  X, W, ATop, ARight: Integer;
  S: string;
begin
  if ColorToRGB(Self.Color) <> ColorToRGB(clBtnFace) then
    Canvas.Pen.Color := clBtnFace
  else
    Canvas.Pen.Color := clBtnShadow;
  LastIndex := FDisplayValues.Count - 1;
  TxtWidth := Canvas.TextWidth('M');
  ATop := Max(0, (HeightOf(R) - Canvas.TextHeight('Xy')) div 2);
  ARight := R.Right;
  Inc(R.Left, ALeft);
  for I := 0 to LastIndex do
  begin
    S := FDisplayValues[I];
    W := LoWord(Longint(FDisplayValues.Objects[I]));
    if I < LastIndex then
      W := W * TxtWidth + 4
    else
      W := ARight - R.Left;
    X := 2;
    R.Right := R.Left + W;
    case TAlignment(HiWord(Longint(FDisplayValues.Objects[I]))) of
      taRightJustify:
        X := W - Canvas.TextWidth(S) - 3;
      taCenter:
        X := (W - Canvas.TextWidth(S)) div 2;
    end;
    Canvas.TextRect(R, R.Left + Max(0, X), ATop, S);
    Inc(R.Left, W);
    if I < LastIndex then
    begin
      Canvas.MoveTo(R.Right, R.Top);
      Canvas.LineTo(R.Right, R.Bottom);
      Inc(R.Left);
    end;
    if R.Left >= ARight then
      Break;
  end;
end;

procedure TJvDBLookupCombo.Paint;
const
  TransColor: array [Boolean] of TColor = (clBtnFace, clWhite);
var
  W, X, Flags, TextMargin: Integer;
  AText: string;
  Selected, DrawList, IsEmpty: Boolean;
  R, ImageRect: TRect;
  Image: TGraphic;
  Bmp: TBitmap;
  Alignment: TAlignment;
  {$IFNDEF WIN32}
  Target: TRect;
  {$ENDIF}
  {$IFDEF JVCLThemesEnabled}
  State: TThemedComboBox;
  Details: TThemedElementDetails;
  {$ENDIF}
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  Selected := FFocused and not FListVisible {$IFDEF WIN32} and  not (csPaintCopy in ControlState){$ENDIF};
  if Selected then
  begin
    Canvas.Font.Color := clHighlightText;
    Canvas.Brush.Color := clHighlight;
  end
  else
  if not Enabled and NewStyleControls then
    Canvas.Font.Color := clGrayText;
  AText := inherited Text;
  Alignment := FAlignment;
  Image := nil;
  IsEmpty := False;
  DrawList := DisplayAllFields;
  {$IFDEF WIN32}
  if (csPaintCopy in ControlState) and (FDataField <> nil) then
  begin
    DrawList := False;
    AText := FDataField.DisplayText;
    Alignment := FDataField.Alignment;
  end;
  {$ENDIF}
  TextMargin := 0;
  if FListVisible then
  begin
    DrawList := False;
    if FDataList.FSearchText <> '' then
    begin
      AText := FDataList.FSearchText;
    end
    else
    begin
      if FDataList.ValueIsEmpty(FDataList.Value) then
      begin
        AText := DisplayEmpty;
        IsEmpty := True;
        Image := GetPicture(False, True, TextMargin);
      end
      else
    if FDataList.FKeyField.AsString = FDataList.Value then
      begin
        AText := FDataList.FDisplayField.DisplayText;
        Image := FDataList.GetPicture(False, False, TextMargin);
      end
      else
      begin
        Image := GetPicture(True, False, TextMargin);
      end;
    end;
  end
  else
  begin
    {$IFDEF WIN32}
    if csPaintCopy in ControlState then
      Image := nil
    else
    {$ENDIF}
    begin
      IsEmpty := ValueIsEmpty(Value);
      Image := GetPicture(True, IsEmpty, TextMargin);
    end;
  end;
  {$IFDEF COMPILER4_UP}
  if UseRightToLeftAlignment then
    ChangeBiDiModeAlignment(Alignment);
  {$ENDIF}
  W := ClientWidth - FButtonWidth;
  if W > 4 then
  begin
    SetRect(R, 1, 1, W - 1, ClientHeight - 1);
    {$IFNDEF WIN32}
    InflateRect(R, -1, -1);
    {$ENDIF}
    if TextMargin > 0 then
      Inc(TextMargin);
    X := 0 {2} + TextMargin; // Polaris
    if not (FListVisible and (FDataList.FSearchText <> '')) and not DrawList then
      case Alignment of
        taRightJustify:
          X := W - Canvas.TextWidth(AText) - 6;
        taCenter:
          X := (W + TextMargin - Canvas.TextWidth(AText)) div 2;
      end;
    Bmp := TBitmap.Create;
    try
      with Bmp.Canvas do
      begin
        Font := Self.Canvas.Font;
        Brush := Self.Canvas.Brush;
        Pen := Self.Canvas.Pen;
      end;
      {$IFDEF COMPILER4_UP}
      if BiDiMode = bdRightToLeft then
      begin
        Inc(X, FButtonWidth);
        Inc(R.Left, FButtonWidth);
        R.Right := ClientWidth;
      end;
      if SysLocale.MiddleEast then
      begin
        TControlCanvas(Self.Canvas).UpdateTextFlags;
        Bmp.Canvas.TextFlags := Self.Canvas.TextFlags;
      end;
      {$ENDIF}
      Bmp.Width := WidthOf(R);
      Bmp.Height := HeightOf(R);
      ImageRect := Rect(0, 0, WidthOf(R), HeightOf(R));
      if DrawList and (ListStyle = lsFixed) and (FDisplayValues <> nil) and
        (FDisplayValues.Count > 0) then
      begin
        if IsEmpty then
        begin
          AText := DisplayEmpty;
          Bmp.Canvas.TextRect(ImageRect, X, Max(0, (HeightOf(R) -
            Canvas.TextHeight(AText)) div 2), AText);
        end
        else
          PaintDisplayValues(Bmp.Canvas, ImageRect, TextMargin);
      end
      else
      begin
        Bmp.Canvas.TextRect(ImageRect, X, Max(0, (HeightOf(R) -
          Canvas.TextHeight(AText)) div 2), AText);
      end;
      if Image <> nil then
      begin
        ImageRect.Right := ImageRect.Left + TextMargin + 2;
        DrawPicture(Bmp.Canvas, ImageRect, Image);
      end;
      Canvas.Draw(R.Left, R.Top, Bmp);
    finally
      Bmp.Free;
    end;
    if Selected then
      Canvas.DrawFocusRect(R);
  end;
  SetRect(R, W, 0, ClientWidth, ClientHeight);
  {$IFDEF COMPILER4_UP}
  if BiDiMode = bdRightToLeft then
  begin
    R.Left := 0;
    R.Right := FButtonWidth;
  end;
  {$ENDIF}
  {$IFDEF WIN32}
  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
  begin
    if (not FListActive) or (not Enabled) or ReadOnly then
      State := tcDropDownButtonDisabled
    else
    if FPressed then
      State := tcDropDownButtonPressed
    else
    if FOver and not FListVisible then
      State := tcDropDownButtonHot
    else
      State := tcDropDownButtonNormal;
    Details := ThemeServices.GetElementDetails(State);
    ThemeServices.DrawElement(Canvas.Handle, Details, R);
  end
  else
  {$ENDIF JVCLThemesEnabled}
  begin
    if not FListActive or not Enabled or ReadOnly then
      Flags := DFCS_SCROLLCOMBOBOX or DFCS_INACTIVE
    else
    if FPressed then
      Flags := DFCS_SCROLLCOMBOBOX or DFCS_FLAT or DFCS_PUSHED
    else
      Flags := DFCS_SCROLLCOMBOBOX;
    DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, Flags);
  end;
  {$ELSE}
  if NewStyleControls then
  begin
    InflateRect(R, -1, -1);
    Dec(R.Left);
  end
  else
  begin
    InflateRect(R, 1, 1);
    Inc(R.Left);
  end;
  R := DrawButtonFace(Canvas, R, 1, bsWin31, False, FPressed, False);
  { draw button glyph }
  if (not FListActive) or (not Enabled) or ReadOnly then
    Bmp := FBtnDisabled
  else
    Bmp := FBtnGlyph;
  Target := Bounds(R.Left, R.Top, Bmp.Width, Bmp.Height);
  OffsetRect(Target, ((R.Right - R.Left) div 2) - (Bmp.Width div 2),
    ((R.Bottom - R.Top) div 2) - (Bmp.Height div 2));
  { Canvas.Draw(Target.Left, Target.Top, Bmp); }
  DrawBitmapTransparent(Canvas, Target.Left, Target.Top, Bmp,
    TransColor[Bmp = FBtnGlyph]);
  {$ENDIF WIN32}
end;

procedure TJvDBLookupCombo.ResetField;
begin
  if FListVisible then
    CloseUp(False);
  inherited ResetField;
  UpdateCurrentImage;
  Invalidate;
end;

procedure TJvDBLookupCombo.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TJvDBLookupCombo.TrackButton(X, Y: Integer);
var
  NewState: Boolean;
begin
  NewState := PtInRect(Rect(ClientWidth - FButtonWidth, 0, ClientWidth,
    ClientHeight), Point(X, Y));
  if FPressed <> NewState then
  begin
    FPressed := NewState;
    Repaint;
  end;
end;

procedure TJvDBLookupCombo.UpdateDisplayEmpty(const Value: string);
begin
  if Text = FDisplayEmpty then
    inherited Text := Value;
end;

procedure TJvDBLookupCombo.Click;
begin
  inherited Click;
  Change;
end;

procedure TJvDBLookupCombo.CMCancelMode(var Msg: TCMCancelMode);
begin
  if (Msg.Sender <> Self) and (Msg.Sender <> FDataList) then
    CloseUp(False);
end;

{$IFDEF WIN32}

procedure TJvDBLookupCombo.CMCtl3DChanged(var Msg: TMessage);
begin
  if NewStyleControls then
  begin
    RecreateWnd;
    if not (csReading in ComponentState) and (Height < GetMinHeight) then
      Height := GetMinHeight;
  end;
  inherited;
end;

procedure TJvDBLookupCombo.CNKeyDown(var Msg: TWMKeyDown);
begin
  if not (csDesigning in ComponentState) then
    if (Msg.CharCode in [VK_RETURN, VK_ESCAPE]) and FListVisible and
      FLookupMode and FDataLink.DataSourceFixed then
    begin
      CloseUp(Msg.CharCode = VK_RETURN);
      Msg.Result := 1;
      Exit;
    end;
  inherited;
end;

{$ENDIF WIN32}

procedure TJvDBLookupCombo.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  if not (csReading in ComponentState) then
    Height := Max(Height, GetMinHeight);
end;

{$IFDEF JVCLThemesEnabled}
procedure TJvDBLookupCombo.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  {Windows XP themes use hot track states, hence we have to update the drop down button.}
  if ThemeServices.ThemesEnabled and not FOver and not (csDesigning in ComponentState) then
  begin
    FOver := True;
    Invalidate;
  end;
end;

procedure TJvDBLookupCombo.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if ThemeServices.ThemesEnabled and FOver then
  begin
    FOver := False;
    Invalidate;
  end;
end;
{$ENDIF JVCLThemesEnabled}

procedure TJvDBLookupCombo.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

{$IFDEF WIN32}
procedure TJvDBLookupCombo.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Integer(FDataLink);
end;
{$ENDIF}

procedure TJvDBLookupCombo.WMCancelMode(var Msg: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TJvDBLookupCombo.WMGetDlgCode(var Msg: TMessage);
begin
  inherited;
  Msg.Result := DLGC_BUTTON or DLGC_WANTALLKEYS or DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TJvDBLookupCombo.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  CloseUp(False);
end;

procedure TJvDBLookupCombo.WMSetCursor(var Msg: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  with ClientRect do
    if PtInRect(Bounds(Right - FButtonWidth, Top, FButtonWidth, Bottom - Top),
      ScreenToClient(P)) then
      {$IFDEF WIN32}
      Windows.SetCursor(LoadCursor(0, IDC_ARROW))
      {$ELSE}
      WinProcs.SetCursor(LoadCursor(0, IDC_ARROW))
      {$ENDIF}
    else
      inherited;
end;

procedure TJvDBLookupCombo.WMSize(var Msg: TWMSize);
begin
  inherited;
  if not (csReading in ComponentState) and (Height < GetMinHeight) then
    Height := GetMinHeight
  else
  begin
    if csDesigning in ComponentState then
      FDataList.SetBounds(0, Height + 1, 10, 10);
  end;
end;

{$IFDEF COMPILER4_UP}
procedure TJvDBLookupCombo.CMBiDiModeChanged(var Msg: TMessage);
begin
  inherited;
  FDataList.BiDiMode := BiDiMode;
end;
{$ENDIF}

//=== TJvPopupDataWindow =====================================================

constructor TJvPopupDataWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditor := TWinControl(AOwner);
  Visible := False;
  Parent := FEditor;
  OnMouseUp := PopupMouseUp;
end;

procedure TJvPopupDataWindow.InvalidateEditor;
var
  R: TRect;
begin
  if FEditor is TJvCustomComboEdit then
    with TJvComboEdit(FEditor) do
      SetRect(R, 0, 0, ClientWidth - Button.Width - 2, ClientHeight + 1)
  else
    R := FEditor.ClientRect;
  InvalidateRect(FEditor.Handle, @R, False);
  UpdateWindow(FEditor.Handle);
end;

procedure TJvPopupDataWindow.Click;
begin
  inherited Click;
  if Value <> '' then
    with TJvLookupEdit(FEditor) do
      if not (FChanging or ReadOnly) then
      begin
        FChanging := True;
        try
          Text := Self.DisplayValue;
          if AutoSelect then
            SelectAll;
        finally
          FChanging := False;
        end;
      end;
  InvalidateEditor;
end;

procedure TJvPopupDataWindow.DisplayValueChanged;
begin
  if not FLockPosition then
    if FListActive then
    begin
      if LocateDisplay then
        FValue := FKeyField.AsString
      else
      begin
        FLookupLink.DataSet.First;
        FValue := EmptyValue;
      end;
    end
    else
      FValue := FEmptyValue;
end;

procedure TJvPopupDataWindow.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  InvalidateEditor;
end;

procedure TJvPopupDataWindow.PopupMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(Self.ClientRect, Point(X, Y)));
end;

procedure TJvPopupDataWindow.CloseUp(Accept: Boolean);
begin
  if Assigned(FCloseUp) then
    FCloseUp(Self, Accept);
end;

function TJvPopupDataWindow.GetPicture(Current, Empty: Boolean;
  var TextMargin: Integer): TGraphic;
begin
  TextMargin := 0;
  Result := nil;
  if Assigned(FOnGetImage) then
    FOnGetImage(FEditor, Empty, Result, TextMargin);
end;

procedure TJvPopupDataWindow.Hide;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
end;

procedure TJvPopupDataWindow.Show(Origin: TPoint);
begin
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;
end;

//=== TJvLookupEdit ==========================================================

constructor TJvLookupEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDropDownCount := 8;
  FPopupOnlyLocate := True;
  ControlState := ControlState + [csCreating];
  try
    FPopup := TJvPopupDataWindow.Create(Self);
    TJvPopupDataWindow(FPopup).OnCloseUp := PopupCloseUp;
    GlyphKind := gkDropDown; { force update }
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

destructor TJvLookupEdit.Destroy;
begin
  if FPopup <> nil then
    with TJvPopupDataWindow(FPopup) do
    begin
      OnCloseUp := nil;
      OnGetImage := nil;
    end;
  FPopup.Free;
  FPopup := nil;
  inherited Destroy;
end;

procedure TJvLookupEdit.SetDropDownCount(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value > 50 then
    Value := 50;
  FDropDownCount := Value;
end;

function TJvLookupEdit.GetListStyle: TLookupListStyle;
begin
  Result := TJvPopupDataWindow(FPopup).ListStyle;
end;

procedure TJvLookupEdit.SetListStyle(Value: TLookupListStyle);
begin
  TJvPopupDataWindow(FPopup).ListStyle := Value;
end;

function TJvLookupEdit.GetFieldsDelimiter: Char;
begin
  Result := TJvPopupDataWindow(FPopup).FieldsDelimiter;
end;

procedure TJvLookupEdit.SetFieldsDelimiter(Value: Char);
begin
  TJvPopupDataWindow(FPopup).FieldsDelimiter := Value;
end;

function TJvLookupEdit.GetLookupDisplay: string;
begin
  Result := TJvPopupDataWindow(FPopup).LookupDisplay;
end;

procedure TJvLookupEdit.SetLookupDisplay(const Value: string);
begin
  TJvPopupDataWindow(FPopup).LookupDisplay := Value;
end;

function TJvLookupEdit.GetDisplayIndex: Integer;
begin
  Result := TJvPopupDataWindow(FPopup).LookupDisplayIndex;
end;

procedure TJvLookupEdit.SetDisplayIndex(Value: Integer);
begin
  TJvPopupDataWindow(FPopup).LookupDisplayIndex := Value;
end;

function TJvLookupEdit.GetLookupField: string;
begin
  Result := TJvPopupDataWindow(FPopup).LookupField;
end;

procedure TJvLookupEdit.SetLookupField(const Value: string);
begin
  TJvPopupDataWindow(FPopup).LookupField := Value;
end;

function TJvLookupEdit.GetLookupSource: TDataSource;
begin
  Result := TJvPopupDataWindow(FPopup).LookupSource;
end;

procedure TJvLookupEdit.SetLookupSource(Value: TDataSource);
begin
  TJvPopupDataWindow(FPopup).LookupSource := Value;
end;

function TJvLookupEdit.GetOnGetImage: TGetImageEvent;
begin
  Result := TJvPopupDataWindow(FPopup).OnGetImage;
end;

procedure TJvLookupEdit.SetOnGetImage(Value: TGetImageEvent);
begin
  TJvPopupDataWindow(FPopup).OnGetImage := Value;
end;

function TJvLookupEdit.GetLookupValue: string;
begin
  TJvPopupDataWindow(FPopup).DisplayValue := Text;
  Result := TJvPopupDataWindow(FPopup).Value;
end;

procedure TJvLookupEdit.SetLookupValue(const Value: string);
begin
  TJvPopupDataWindow(FPopup).Value := Value;
  Text := TJvPopupDataWindow(FPopup).DisplayValue;
end;

procedure TJvLookupEdit.ShowPopup(Origin: TPoint);
begin
  TJvPopupDataWindow(FPopup).Show(Origin);
end;

procedure TJvLookupEdit.HidePopup;
begin
  TJvPopupDataWindow(FPopup).Hide;
end;

procedure TJvLookupEdit.PopupDropDown(DisableEdit: Boolean);
begin
  if not (ReadOnly or PopupVisible) then
  begin
    if Assigned(FOnDropDown) then
      FOnDropDown(Self);
    with TJvPopupDataWindow(FPopup) do
    begin
      Color := Self.Color;
      Font := Self.Font;
      if FDropDownWidth > 0 then
        Width := FDropDownWidth
      else
      if FDropDownWidth < 0 then
        Width := Max(Self.Width, GetWindowWidth)
      else
        Width := Self.Width;
      ReadOnly := Self.ReadOnly;
      RowCount := FDropDownCount;
    end;
  end;
  inherited PopupDropDown(False);
end;

procedure TJvLookupEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_PRIOR, VK_NEXT, VK_UP, VK_DOWN]) and PopupVisible then
  begin
    TJvPopupDataWindow(FPopup).KeyDown(Key, Shift);
    Key := 0;
  end;
  inherited KeyDown(Key, Shift);
  FIgnoreChange := (SelLength > 0) or (Key = VK_BACK);
  if not (PopupVisible or ReadOnly) and (Key in [VK_UP, VK_DOWN]) and
    (Shift = []) then
  begin
    with TJvPopupDataWindow(FPopup) do
    begin
      KeyDown(Key, Shift);
      if Value <> EmptyValue then
        Key := 0;
    end;
  end;
end;

procedure TJvLookupEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  FIgnoreChange := (SelLength > 0) or (Key = Char(VK_BACK));
end;

procedure TJvLookupEdit.Change;
begin
  if PopupOnlyLocate or PopupVisible then
    inherited Change
  else
  begin
    PopupChange;
    DoChange;
  end;
end;

procedure TJvLookupEdit.PopupChange;
var
  S: string;
  Len: Integer;
begin
  if FChanging or FIgnoreChange or ReadOnly then
  begin
    FIgnoreChange := False;
    Exit;
  end;
  FChanging := True;
  try
    S := Text;
    if TJvPopupDataWindow(FPopup).SearchText(S) then
    begin
      Len := Length(Text);
      Text := TJvPopupDataWindow(FPopup).DisplayValue;
      SelStart := Len;
      SelLength := Length(Text) - Len;
    end
    else
      with TJvPopupDataWindow(FPopup) do
        Value := EmptyValue;
  finally
    FChanging := False;
  end;
end;

{$IFDEF WIN32}
procedure TJvLookupEdit.SetPopupValue(const Value: Variant);
{$ELSE}
procedure TJvLookupEdit.SetPopupValue(const Value: string);
{$ENDIF}
begin
  {$IFDEF WIN32}
  if VarIsNull(Value) or VarIsEmpty(Value) then
    TJvPopupDataWindow(FPopup).Value := TJvPopupDataWindow(FPopup).EmptyValue
  else
  {$ENDIF}
    TJvPopupDataWindow(FPopup).DisplayValue := Value;
end;

{$IFDEF WIN32}
function TJvLookupEdit.GetPopupValue: Variant;
{$ELSE}
function TJvLookupEdit.GetPopupValue: string;
{$ENDIF}
begin
  with TJvPopupDataWindow(FPopup) do
    if Value <> EmptyValue then
      Result := DisplayValue
    else
      Result := Self.Text;
end;

{$IFDEF WIN32}
function TJvLookupEdit.AcceptPopup(var Value: Variant): Boolean;
{$ELSE}
function TJvLookupEdit.AcceptPopup(var Value: string): Boolean;
{$ENDIF}
begin
  Result := True;
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self);
end;

end.

