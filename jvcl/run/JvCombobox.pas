{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCombobox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCombobox;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Graphics, Controls, Forms, StdCtrls, Menus,
  JvCheckListBox, JvExStdCtrls, JvDataProvider, JvMaxPixel,
  JvToolEdit;

type
  TJvCustomComboBox = class;

  { This class will be used for the Items property of the combo box.

    If a provider is active at the combo box, this list will keep the strings stored in an internal
    list.

    Whenever an item is added to the list the provider will be deactivated and the list will be
    handled by the combo box as usual. }
  TJvComboBoxStrings = class(TCustomComboBoxStrings)
  private
    FInternalList: TStringList;
    FUseInternal: Boolean;
    FUpdating: Boolean;
    FDestroyCnt: Integer;
    function GetInternalList: TStrings;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure SetWndDestroying(Destroying: Boolean);
    function GetComboBox: TJvCustomComboBox;
    procedure SetComboBox(Value: TJvCustomComboBox);
    property ComboBox: TJvCustomComboBox read GetComboBox write SetComboBox;
    property InternalList: TStrings read GetInternalList;
    property UseInternal: Boolean read FUseInternal write FUseInternal;
    property Updating: Boolean read FUpdating;
    property DestroyCount: Integer read FDestroyCnt;
  public
    constructor Create; {$IFDEF RTL200_UP}override;{$ENDIF RTL200_UP}
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure MakeListInternal; virtual;
    procedure ActivateInternal; virtual;
  end;

  TJvComboBoxStringsClass = class of TJvComboBoxStrings;

  TJvComboBoxMeasureStyle = (cmsStandard, cmsAfterCreate, cmsBeforeDraw);

  TJvCustomComboBox = class(TJvExCustomComboBox)
  private
    FMaxPixel: TJvMaxPixel;
    FReadOnly: Boolean;
    FConsumerSvc: TJvDataConsumer;
    FProviderToggle: Boolean;
    FProviderIsActive: Boolean;
    FIsFixedHeight: Boolean;
    FMeasureStyle: TJvComboBoxMeasureStyle;
    FLastSetItemHeight: Integer;
    FEmptyValue: string;
    FIsEmptyValue: Boolean;
    FEmptyFontColor: TColor;
    FOldFontColor: TColor;
    FDropDownWidth: Integer;
    procedure SetEmptyValue(const Value: string);
    procedure MaxPixelChanged(Sender: TObject);
    procedure SetReadOnly(const Value: Boolean);
    procedure ReadCtl3D(Reader: TReader);
    procedure ReadParentCtl3D(Reader: TReader);
    procedure CNCommand(var Msg: TWMCommand); message CN_COMMAND;
    procedure CNMeasureItem(var Msg: TWMMeasureItem); message CN_MEASUREITEM;
    procedure WMInitDialog(var Msg: TWMInitDialog); message WM_INITDIALOG;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Msg: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    function GetFlat: Boolean;
    function GetParentFlat: Boolean;
    procedure SetFlat(const Value: Boolean);
    procedure SetParentFlat(const Value: Boolean);
    procedure SetDropDownWidth(Value: Integer);
  protected
    function IsItemHeightStored: Boolean; {$IFDEF COMPILER14_UP} override; {$ENDIF}
    function GetText: TCaption; virtual;
    procedure SetText(const Value: TCaption); reintroduce; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoEmptyValueEnter; virtual;
    procedure DoEmptyValueExit; virtual;
    procedure CreateWnd; override;
    function GetItemsClass: TCustomComboBoxStringsClass; override;
    procedure KeyPress(var Key: Char); override;
    procedure SetItemHeight(Value: Integer); override;
    function GetMeasureStyle: TJvComboBoxMeasureStyle;
    procedure SetMeasureStyle(Value: TJvComboBoxMeasureStyle);
    procedure PerformMeasure;
    procedure PerformMeasureItem(Index: Integer; var Height: Integer); virtual;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    function IsProviderSelected: Boolean;
    procedure DeselectProvider;
    procedure UpdateItemCount;
    function HandleFindString(StartIndex: Integer; Value: string; ExactMatch: Boolean): Integer;
    procedure Loaded; override;
    procedure SetConsumerService(Value: TJvDataConsumer);
    procedure ConsumerServiceChanged(Sender: TJvDataConsumer; Reason: TJvDataConsumerChangeReason);
    procedure ConsumerSubServiceCreated(Sender: TJvDataConsumer; SubSvc: TJvDataConsumerAggregatedObject);
    property Provider: TJvDataConsumer read FConsumerSvc write SetConsumerService;
    property IsFixedHeight: Boolean read FIsFixedHeight;
    property MeasureStyle: TJvComboBoxMeasureStyle read GetMeasureStyle write SetMeasureStyle
      default cmsStandard;
    property MaxPixel: TJvMaxPixel read FMaxPixel write FMaxPixel;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Text: TCaption read GetText write SetText;
    property EmptyValue: string read FEmptyValue write SetEmptyValue;
    property EmptyFontColor: TColor read FEmptyFontColor write FEmptyFontColor default clGrayText;
    property Flat: Boolean read GetFlat write SetFlat default False;
    property ParentFlat: Boolean read GetParentFlat write SetParentFlat default True;
    property DropDownWidth: Integer read FDropDownWidth write SetDropDownWidth default 0;
    property ItemHeight stored IsItemHeightStored;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure DestroyWnd; override;
    procedure WndProc(var Msg: TMessage); override;
    function GetItemCount: Integer; override; 
    procedure DefineProperties(Filer: TFiler);override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetItemText(Index: Integer): string; virtual;
    function SearchExactString(const Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchPrefix(const Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchSubString(const Value: string; CaseSensitive: Boolean = True): Integer;
    function DeleteExactString(const Value: string; All: Boolean; CaseSensitive: Boolean = True): Integer;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvComboBox = class(TJvCustomComboBox)
  published
    property Align;
    property HintColor;
    property MaxPixel;
    property AutoComplete default True;
    property AutoDropDown default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property EmptyValue;
    property EmptyFontColor;
    property Flat;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property MaxLength;
    property MeasureStyle;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFlat;
    property ParentFont;
    property ParentShowHint;
    property Provider;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
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
    property Items; { Must be published after OnMeasureItem }
    property ItemIndex default -1;  { Must be published after Items (see Mantis 3512) }
    property DropDownWidth;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;

  TJvCHBQuoteStyle = (qsNone, qsSingle, qsDouble);

  TJvCustomCheckedComboBox = class(TJvCustomComboEdit)
  private
    FCapSelAll: string;
    FCapDeselAll: string;
    FCapInvertAll: string;
    FListBox: TJvCheckListBox;
    FSelectAll: TMenuItem;
    FDeselectAll: TMenuItem;
    FInvertAll: TMenuItem;
    FNoFocusColor: TColor;
    FSorted: Boolean;
    FQuoteStyle: TJvCHBQuoteStyle;
    FCheckedCount: Integer;
    FCheckedCountValid: Boolean;
    FColumns: Integer;
    FDropDownLines: Integer;
    FDelimiter: Char;
    FIgnoreChange: Boolean;
    FOrderedText: Boolean;
    FOrgListBoxWndProc: TWndMethod;
    FUpdateCheckedTextSent: Boolean;
    FKeepCheckedState: Boolean;
    procedure SetItems(AItems: TStrings);
    procedure ToggleOnOff(Sender: TObject);
    procedure KeyListBox(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ContextListBox(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure ListBoxWndProc(var Msg: TMessage);
    procedure ItemsChange;
    procedure SetSorted(Value: Boolean);
    procedure AdjustHeight;
    procedure SetNoFocusColor(Value: TColor);
    procedure SetColumns(Value: Integer);
    procedure SetChecked(Index: Integer; Checked: Boolean);
    procedure SetDropDownLines(Value: Integer);
    function GetChecked(Index: Integer): Boolean;
    function GetItemEnabled(Index: Integer): Boolean;
    procedure SetItemEnabled(Index: Integer; const Value: Boolean);
    function GetState(Index: Integer): TCheckBoxState;
    procedure SetState(Index: Integer; const Value: TCheckBoxState);
    procedure SetDelimiter(const Value: Char);
    function IsStoredCapDeselAll: Boolean;
    function IsStoredCapSelAll: Boolean;
    procedure ChangeText(const NewText: string);
    procedure SetOrderedText(const Value: Boolean);
    function GetOrderedTextValue: string;
    function GetItems: TStrings;
    function GetCheckedCount: Integer;
    function GetHeader(Index: Integer): Boolean;
    procedure SetHeader(Index: Integer; const Value: Boolean);
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure AdjustSize; override;
    procedure CreatePopup; override;
    procedure Change; override;
    function AcceptPopup(var Value: Variant): Boolean; override;

    property ListBox: TJvCheckListBox read FListBox;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure SetUnCheckedAll(Sender: TObject = nil);
    procedure SetCheckedAll(Sender: TObject = nil);
    procedure SetInvertAll(Sender: TObject = nil);
    function IsChecked(Index: Integer): Boolean;
    function GetText: string;
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property CheckedCount: Integer read GetCheckedCount;
    property ItemEnabled[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
    property Header[Index: Integer]: Boolean read GetHeader write SetHeader;

    property Items: TStrings read GetItems write SetItems;
    property CapSelectAll: string read FCapSelAll write FCapSelAll stored IsStoredCapSelAll;
    property CapDeSelectAll: string read FCapDeselAll write FCapDeselAll stored IsStoredCapDeselAll;
    property CapInvertAll: string read FCapInvertAll write FCapInvertAll;
    property NoFocusColor: TColor read FNoFocusColor write SetNoFocusColor;
    property Sorted: Boolean read FSorted write SetSorted default False;
    property QuoteStyle: TJvCHBQuoteStyle read FQuoteStyle write FQuoteStyle default qsNone;
    property Columns: Integer read FColumns write SetColumns default 0;
    property DropDownLines: Integer read FDropDownLines write SetDropDownLines default 6;
    property Delimiter: Char read FDelimiter write SetDelimiter default ',';
    property OrderedText: Boolean read FOrderedText write SetOrderedText default False;
    property KeepCheckedState: Boolean read FKeepCheckedState write FKeepCheckedState default False;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvCheckedComboBox = class(TJvCustomCheckedComboBox)
  published
    property Items;
    property CapSelectAll;
    property CapDeSelectAll;
    property CapInvertAll;
    property NoFocusColor default clWindow;
    property Sorted;
    property QuoteStyle;
    property Columns;
    property DropDownLines;
    property Delimiter;

    property Align;
    property HintColor;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color default clWindow;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Flat;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFlat;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly default True;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
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
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;

    {$IFDEF COMPILER14_UP}
    property Touch;
    {$ENDIF COMPILER14_UP}
    property TextHint;
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
  Types, SysUtils, Consts, RTLConsts, Variants,
  JvDataProviderIntf, JvItemsSearchs, JvConsts, JvResources;

const
  MinDropLines = 2;
  MaxDropLines = 50;
  WM_UPDATECHECKEDTEXT = WM_USER + 102;

type
  TJvPrivForm = class(TJvPopupWindow)
  protected
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
  end;


//=== Local procedures =======================================================

// examines if string (part) exist in string (source)
// where source is in format part1[,part2]

function PartExist(const Part, Source: string; Delimiter: Char): Boolean;
var
  M: Integer;
  Temp1, Temp2: string;
begin
  Temp1 := Source;
  Result := Part = Temp1;
  while not Result do
  begin
    M := Pos(Delimiter, Temp1);
    if M > 0 then
      Temp2 := Copy(Temp1, 1, M - 1)
    else
      Temp2 := Temp1;
    Result := Part = Temp2;
    if Result or (M = 0) then
      Break;
    Delete(Temp1, 1, M);
  end;
end;

// removes a string (part) from another string (source)
// when source is in format part1[,part2]

function RemovePart(const Part, Source: string; Delimiter: Char): string;
var
  Len, P: Integer;
  S1, S2: string;
begin
  Result := Source;
  S1 := Delimiter + Part + Delimiter;
  S2 := Delimiter + Source + Delimiter;
  P := Pos(S1, S2);
  if P > 0 then
  begin
    Len := Length(Part);
    if P = 1 then
      Result := Copy(Source, P + Len + 1, MaxInt)
    else
    begin
      Result := Copy(S2, 2, P - 1) + Copy(S2, P + Len + 2, MaxInt);
      SetLength(Result, Length(Result) - 1);
    end;
  end;
end;

function Add(const Sub: string; var Str: string; Delimiter: Char): Boolean;
begin
  Result := False;
  if Str = '' then
  begin
    Str := Sub;
    Result := True;
  end
  else
  if not PartExist(Sub, Str, Delimiter) then
  begin
    Str := Str + Delimiter + Sub;
    Result := True;
  end;
end;

function Remove(const Sub: string; var Str: string; Delimiter: Char): Boolean;
var
  Temp: string;
begin
  Result := False;
  if Str <> '' then
  begin
    Temp := RemovePart(Sub, Str, Delimiter);
    if Temp <> Str then
    begin
      Str := Temp;
      Result := True;
    end;
  end;
end;

// added 2000/04/08

function GetFormattedText(Kind: TJvCHBQuoteStyle; const Str: string; Delimiter: Char): string;
var
  S: string;
begin
  Result := Str;
  if Str <> '' then
  begin
    S := Str;
    case Kind of
      qsSingle:
        Result := '''' + StringReplace(S, Delimiter, '''' + Delimiter + '''', [rfReplaceAll]) + '''';
      qsDouble:
        Result := '"' + StringReplace(S, Delimiter, '"' + Delimiter + '"', [rfReplaceAll]) + '"';
    end;
  end;
end;

//=== { TJvCustomCheckedComboBox } ===========================================

constructor TJvCustomCheckedComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDropDownLines := 6;
  FDelimiter := ',';
  FColumns := 0;
  FQuoteStyle := qsNone;
  FCheckedCount := 0;
  FNoFocusColor := clWindow;
  Caption := '';
  FCapSelAll := RsCapSelAll;
  FCapDeselAll := RsCapDeselAll;
  FCapInvertAll := RsCapInvertAll;
  Height := 24;
  Width := 121;

  Color := clWindow;
  ReadOnly := True;

  ShowButton := True;
  ImageKind := ikDropDown;
  AlwaysEnableButton := True;
  AlwaysShowPopup := True;

  Text := '';

  // Create a form with its contents
  FPopup := TJvPrivForm.Create(Self);
  TJvPrivForm(FPopup).OnCloseUp := PopupCloseUp;
  TJvPrivForm(FPopup).FIsFocusable := True;

  // Create CheckListBox
  FListBox := TJvCheckListBox.Create(FPopup);
  FListBox.Parent := FPopup;
  FListBox.BorderStyle := bsNone;
  FListBox.Ctl3D := False;
  FListBox.Columns := FColumns;
  FListBox.Align := alClient;
  FListBox.OnClickCheck := ToggleOnOff;
  FListBox.OnKeyDown := KeyListBox;
  FListBox.OnContextPopup := ContextListBox;
  FOrgListBoxWndProc := FListBox.WindowProc;
  FListBox.WindowProc := ListBoxWndProc;
  TJvPrivForm(FPopup).FActiveControl := FListBox;

  // Create PopUp
  FListBox.PopupMenu := TPopupMenu.Create(FPopup);
  FSelectAll := TMenuItem.Create(FListBox.PopupMenu);
  FSelectAll.Caption := FCapSelAll;
  FSelectAll.OnClick := SetCheckedAll;
  FListBox.PopupMenu.Items.Insert(0, FSelectAll);
  FDeselectAll := TMenuItem.Create(FListBox.PopupMenu);
  FDeselectAll.Caption := FCapDeselAll;
  FDeselectAll.OnClick := SetUnCheckedAll;
  FListBox.PopupMenu.Items.Insert(1, FDeselectAll);

  FInvertAll := TMenuItem.Create(FListBox.PopupMenu);
  FInvertAll.Caption := FCapInvertAll;
  FInvertAll.OnClick := SetInvertAll;
  FListBox.PopupMenu.Items.Insert(2, FInvertAll);
end;

destructor TJvCustomCheckedComboBox.Destroy;
begin
  FPopup.Free;
  FPopup := nil;
  inherited Destroy;
end;

function TJvCustomCheckedComboBox.AcceptPopup(var Value: Variant): Boolean;
begin
  Result := False;
end;

procedure TJvCustomCheckedComboBox.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(HWND_DESKTOP);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(HWND_DESKTOP, DC);
  if Ctl3D then
    I := 8
  else
    I := 6;
  I := GetSystemMetrics(SM_CYBORDER) * I;
  Height := Metrics.tmHeight + I;
end;

procedure TJvCustomCheckedComboBox.AdjustSize;
begin
  inherited AdjustSize;
  AdjustHeight;
end;

procedure TJvCustomCheckedComboBox.Clear;
begin
  FListBox.Clear;
  inherited Clear;
end;

procedure TJvCustomCheckedComboBox.ContextListBox(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  PopupMenu: TPopupMenu;
begin
  { We basically need this code because the standard Delphi code sends a
    SendCancelMode(nil) that will close the popup if the popup has not the focus.
    But this also gives us a change to position the popup when Shift + F10
    is used (Thus if InvalidPoint(MousePos) = true)
  }
  PopupMenu := FListBox.PopupMenu;
  if (PopupMenu <> nil) and PopupMenu.AutoPopup then
  begin
    SendCancelMode(FListBox);
    PopupMenu.PopupComponent := FListBox;
    if (MousePos.X = -1) and (MousePos.Y = -1) then // ahuser: InvalidPoint is not supported by Delphi 5
      with FListBox do
        if ItemIndex >= 0 then
          MousePos := Point(Width div 2, ItemHeight * (ItemIndex + 1))
        else
          MousePos := Point(Width div 2, Height div 2);

    MousePos := FListBox.ClientToScreen(MousePos);
    PopupMenu.Popup(MousePos.X, MousePos.Y);
    Handled := True;
  end;
end;

procedure TJvCustomCheckedComboBox.CreatePopup;
var
  DisplayDropDownLines: Integer;
begin
  //Click;
  if fColumns > 1 then
    // determine the real lines needed if FColumns > 1
    DisplayDropDownLines := FListBox.Items.Count div FColumns + 1
  else
    // determine the real lines needed if FColumns = 1
    DisplayDropDownLines := FListBox.Items.Count + 1;

  if DisplayDropDownLines > FDropDownLines then
    // If the actual lines > value of property "DropDownLines", revert to property value
    DisplayDropDownLines := FDropDownLines;

  // adjust "DisplayDropDownLines" according to Min and Max values
  if DisplayDropDownLines < MinDropLines then
    DisplayDropDownLines := MinDropLines;
  if DisplayDropDownLines > MaxDropLines then
    DisplayDropDownLines := MaxDropLines;

  FSelectAll.Caption := FCapSelAll;
  FDeselectAll.Caption := FCapDeselAll;
  FInvertAll.Caption := FCapInvertAll;

  with TJvPrivForm(FPopup) do
  begin
    Font := Self.Font;
    Width := Self.Width;
    // use the current "DisplayDropDownLines" to determine height of window
    Height := (DisplayDropDownLines * FListBox.ItemHeight + 4 { FEdit.Height });
  end;
end;

procedure TJvCustomCheckedComboBox.Change;
begin
  if not FIgnoreChange then
  begin
    DoChange;
    FListBox.Refresh;
  end;
end;

procedure TJvCustomCheckedComboBox.DoEnter;
begin
  Color := clWindow;
  inherited DoEnter;
end;

procedure TJvCustomCheckedComboBox.DoExit;
begin
  Color := FNoFocusColor;
  inherited DoExit;
end;

function TJvCustomCheckedComboBox.GetChecked(Index: Integer): Boolean;
begin
  Result := FListBox.Checked[Index];
end;

function TJvCustomCheckedComboBox.GetItemEnabled(Index: Integer): Boolean;
begin
  Result := FListBox.ItemEnabled[Index];
end;

function TJvCustomCheckedComboBox.GetHeader(Index: Integer): Boolean;
begin
  Result := FListBox.Header[Index];
end;

function TJvCustomCheckedComboBox.GetItems: TStrings;
begin
  Result := FListBox.Items;
end;

function TJvCustomCheckedComboBox.GetState(Index: Integer): TCheckBoxState;
begin
  Result := FListBox.State[Index];
end;

function TJvCustomCheckedComboBox.GetText: string;
begin
  if FQuoteStyle = qsNone then
    Result := Text
  else
    Result := GetFormattedText(FQuoteStyle, Text, Delimiter);
end;

function TJvCustomCheckedComboBox.GetOrderedTextValue: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FListBox.Count - 1 do
  begin
    if FListBox.Checked[I] then
    begin
      if Result <> '' then
        Result := Result + FDelimiter;
      Result := Result + FListBox.Items[I];
    end;
  end;
end;

function TJvCustomCheckedComboBox.IsChecked(Index: Integer): Boolean;
begin
  Result := FListBox.Checked[Index];
end;

function TJvCustomCheckedComboBox.IsStoredCapDeselAll: Boolean;
begin
  Result := FCapDeselAll <> RsCapSelAll;
end;

function TJvCustomCheckedComboBox.IsStoredCapSelAll: Boolean;
begin
  Result := FCapSelAll <> RsCapDeselAll;
end;

function TJvCustomCheckedComboBox.GetCheckedCount: Integer;
var
  I, Count: Integer;
begin
  if not FCheckedCountValid then
  begin
    FCheckedCountValid := True;
    Count := 0;
    for I := 0 to Items.Count - 1 do
      if Checked[I] then
        Inc(Count);
    FCheckedCount := Count;
  end;
  Result := FCheckedCount;
end;

procedure TJvCustomCheckedComboBox.ItemsChange;
begin
  FCheckedCountValid := False;
  if KeepCheckedState then
  begin
    if not FUpdateCheckedTextSent then
    begin
      FUpdateCheckedTextSent := True;
      PostMessage(FListBox.Handle, WM_UPDATECHECKEDTEXT, 0, 0);
    end;
  end
  else
  begin // old behavior
    if FCheckedCount > 0 then
      SetUnCheckedAll
    else
      ChangeText('');
  end;
end;

procedure TJvCustomCheckedComboBox.ListBoxWndProc(var Msg: TMessage);
begin
  FOrgListBoxWndProc(Msg);
  case Msg.Msg of
    WM_PAINT, WM_UPDATECHECKEDTEXT:
      begin
        if FUpdateCheckedTextSent then
        begin
          FUpdateCheckedTextSent := False;
          ChangeText(GetOrderedTextValue);
        end;
      end;
    LB_ADDSTRING, LB_DELETESTRING, LB_INSERTSTRING, LB_RESETCONTENT:
      ItemsChange;
  end;
end;

procedure TJvCustomCheckedComboBox.KeyListBox(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Shift * KeyboardShiftStates = []) then
  begin
    PopupCloseUp(Self, False);
    Key := 0;
  end;
end;

procedure TJvCustomCheckedComboBox.SetChecked(Index: Integer; Checked: Boolean);
var
  S: string;
  ChangeData: Boolean;
  WasChecked: Boolean;
begin
  WasChecked := FListBox.Checked[Index]; // throws EListError if Index is invalid
  S := Text;
  ChangeData := False;
  if not WasChecked and Checked then
  begin
    if Add(FListBox.Items[Index], S, Delimiter) then
      ChangeData := True;
  end
  else
  if WasChecked and not Checked then
    if Remove(FListBox.Items[Index], S, Delimiter) then
      ChangeData := True;

  if WasChecked <> Checked then
    if Checked then
      FCheckedCount := FCheckedCount + 1
    else
      FCheckedCount := FCheckedCount - 1;

  if ChangeData then
  begin
    FListBox.Checked[Index] := Checked;
    ChangeText(S);
  end;

  if WasChecked <> Checked then
    Change;
end;

procedure TJvCustomCheckedComboBox.SetCheckedAll(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  S := '';
  for I := 0 to FListBox.Items.Count - 1 do
  begin
    if not FListBox.Checked[I] then
      FListBox.Checked[I] := True;

    if I = 0 then
      S := FListBox.Items[I]
    else
      S := S + Delimiter + FListBox.Items[I];
  end;
  ChangeText(S);
  FCheckedCount := FListBox.Items.Count;
  Repaint;
  Change;
end;

procedure TJvCustomCheckedComboBox.SetInvertAll(Sender: TObject);
var
  I: Integer;
  S: string;
  NewCheckedCount: Integer;
begin
  S := '';
  NewCheckedCount := 0;
  for I := 0 to FListBox.Items.Count - 1 do
  begin
    FListBox.Checked[I] := not FListBox.Checked[I];

    if FListBox.Checked[I] then
    begin
      Inc(NewCheckedCount);

       if S = '' then         
         S := FListBox.Items[I]
       else
         S := S + Delimiter + FListBox.Items[I];
    end;
  end;
  ChangeText(S);

  FCheckedCount := NewCheckedCount;
  Repaint;
  Change;
end;

procedure TJvCustomCheckedComboBox.SetColumns(Value: Integer);
begin
  if FColumns <> Value then
  begin
    FColumns := Value;
    FListBox.Columns := FColumns;
  end;
end;

procedure TJvCustomCheckedComboBox.SetDelimiter(const Value: Char);
var
  I: Integer;
  S: string;
begin
  if Value <> FDelimiter then
  begin
    FDelimiter := Value;
    Text := '';
    S := '';
    for I := 0 to FListBox.Items.Count - 1 do
      if FListBox.Checked[I] then
        if I = 0 then
          S := FListBox.Items[I]
        else
          S := S + Delimiter + FListBox.Items[I];
    ChangeText(S);
  end;
end;

procedure TJvCustomCheckedComboBox.SetDropDownLines(Value: Integer);
begin
  if FDropDownLines <> Value then
    if (Value >= MinDropLines) and (Value <= MaxDropLines) then
      FDropDownLines := Value;
end;

procedure TJvCustomCheckedComboBox.SetHeader(Index: Integer; const Value: Boolean);
begin
  FListBox.Header[Index] := Value;
end;

procedure TJvCustomCheckedComboBox.SetItemEnabled(Index: Integer; const Value: Boolean);
begin
  FListBox.ItemEnabled[Index] := Value;
end;

procedure TJvCustomCheckedComboBox.SetItems(AItems: TStrings);
begin
  FListBox.Items.Assign(AItems);
end;

procedure TJvCustomCheckedComboBox.SetNoFocusColor(Value: TColor);
begin
  if FNoFocusColor <> Value then
  begin
    FNoFocusColor := Value;
    Color := Value;
  end;
end;

procedure TJvCustomCheckedComboBox.SetOrderedText(const Value: Boolean);
begin
  if FOrderedText <> Value then
  begin
    FOrderedText := Value;

    if FOrderedText then
      ChangeText(GetOrderedTextValue);
  end;
end;

procedure TJvCustomCheckedComboBox.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    FListBox.Sorted := FSorted;
  end;
end;

procedure TJvCustomCheckedComboBox.SetState(Index: Integer; const Value: TCheckBoxState);
begin
  FListBox.State[Index] := Value;
end;

procedure TJvCustomCheckedComboBox.SetUnCheckedAll(Sender: TObject);
var
  I: Integer;
begin
  FCheckedCount := 0;
  for I := 0 to FListBox.Items.Count - 1 do
    FListBox.Checked[I] := False;
  ChangeText('');
  Change;
end;

procedure TJvCustomCheckedComboBox.ToggleOnOff(Sender: TObject);
var
  S: string;
begin
  if FListBox.ItemIndex = -1 then
    Exit;
  S := Text;
  if FListBox.Checked[FListBox.ItemIndex] then
  begin
    FCheckedCount := FCheckedCount + 1;
    if not PartExist(FListBox.Items[FListBox.ItemIndex], S, Delimiter) then
    begin
      if not OrderedText then
        Add(FListBox.Items[FListBox.ItemIndex], S, Delimiter)
      else
        S := GetOrderedTextValue;
    end;
  end
  else
  begin
    FCheckedCount := FCheckedCount - 1;
    Remove(FListBox.Items[FListBox.ItemIndex], S, Delimiter);
  end;
  ChangeText(S);
  Change;
end;

procedure TJvCustomCheckedComboBox.ChangeText(const NewText: string);
begin
  FIgnoreChange := True;
  try
    Text := NewText;
  finally
    FIgnoreChange := False;
  end;
end;

//=== { TJvComboBoxStrings } =================================================

constructor TJvComboBoxStrings.Create;
begin
  inherited Create;
  FInternalList := TStringList.Create;
end;

destructor TJvComboBoxStrings.Destroy;
begin
  FreeAndNil(FInternalList);
  inherited Destroy;
end;

procedure TJvComboBoxStrings.ActivateInternal;
var
  S: string;
  Obj: TObject;
  Index: Integer;
begin
  SendMessage(ComboBox.Handle, WM_SETREDRAW, Ord(False), 0);
  try
    InternalList.BeginUpdate;
    try
      SendMessage(ComboBox.Handle, CB_RESETCONTENT, 0, 0);
      while InternalList.Count > 0 do
      begin
        S := InternalList[0];
        Obj := InternalList.Objects[0];
        // Comclt32.dll version 5.0 or later: If CBS_LOWERCASE or CBS_UPPERCASE is set, the Unicode version of
        // CB_ADDSTRING alters the string. If using read-only global memory, this causes the application to fail.
        if ComboBox.CharCase <> ecNormal then
          UniqueString(S);
        Index := SendMessage(ComboBox.Handle, CB_ADDSTRING, 0, LPARAM(PChar(S)));
        if Index < 0 then
          raise EOutOfResources.CreateRes(@SInsertLineError);
        SendMessage(ComboBox.Handle, CB_SETITEMDATA, Index, LPARAM(Obj));
        InternalList.Delete(0);
      end;
    finally
      InternalList.EndUpdate;
    end;
  finally
    if not Updating then
      SendMessage(ComboBox.Handle, WM_SETREDRAW, Ord(True), 0);
    UseInternal := False;
  end;
end;

function TJvComboBoxStrings.Add(const S: string): Integer;
var
  Value: string;
begin
  if (csLoading in ComboBox.ComponentState) and UseInternal then
    Result := InternalList.Add(S)
  else
  begin
    ComboBox.DeselectProvider;
    Value := S;
    // Comclt32.dll version 5.0 or later: If CBS_LOWERCASE or CBS_UPPERCASE is set, the Unicode version of
    // CB_ADDSTRING alters the string. If using read-only global memory, this causes the application to fail.
    if ComboBox.CharCase <> ecNormal then
      UniqueString(Value);
    Result := SendMessage(ComboBox.Handle, CB_ADDSTRING, 0, LPARAM(PChar(Value)));
    if Result < 0 then
      raise EOutOfResources.CreateRes(@SInsertLineError);
  end;
end;

procedure TJvComboBoxStrings.Clear;
var
  S: string;
begin
  if (FDestroyCnt <> 0) and UseInternal then
    Exit;
  if (csLoading in ComboBox.ComponentState) and UseInternal then
    InternalList.Clear
  else
  begin
    S := ComboBox.Text;
    ComboBox.DeselectProvider;
    SendMessage(ComboBox.Handle, CB_RESETCONTENT, 0, 0);
    ComboBox.Text := S;
    ComboBox.Update;
  end;
end;

procedure TJvComboBoxStrings.Delete(Index: Integer);
begin
  if (csLoading in ComboBox.ComponentState) and UseInternal then
    InternalList.Delete(Index)
  else
  begin
    ComboBox.DeselectProvider;
    SendMessage(ComboBox.Handle, CB_DELETESTRING, Index, 0);
  end;
end;

function TJvComboBoxStrings.Get(Index: Integer): string;
var
  S: string;
  Len: Integer;
begin
  if UseInternal then
    Result := InternalList[Index]
  else
  begin
    Len := SendMessage(ComboBox.Handle, CB_GETLBTEXTLEN, Index, 0);
    if Len = CB_ERR then
      Error(SListIndexError, Index);
    SetLength(S, Len);
    if Len > 0 then
      SendMessage(ComboBox.Handle, CB_GETLBTEXT, Index, LPARAM(PChar(S)));
    Result := S;
  end;
end;

function TJvComboBoxStrings.GetComboBox: TJvCustomComboBox;
begin
  Result := TJvCustomComboBox(inherited ComboBox);
end;

function TJvComboBoxStrings.GetCount: Integer;
begin
  if (DestroyCount > 0) and UseInternal then
    Result := 0
  else
  begin
    if UseInternal then
    begin
      Result := InternalList.Count
    end
    else
      Result := SendMessage(ComboBox.Handle, CB_GETCOUNT, 0, 0);
  end;
end;

function TJvComboBoxStrings.GetInternalList: TStrings;
begin
  Result := FInternalList;
end;

function TJvComboBoxStrings.GetObject(Index: Integer): TObject;
begin
  if UseInternal then
    Result := InternalList.Objects[Index]
  else
  begin
    Result := TObject(SendMessage(ComboBox.Handle, CB_GETITEMDATA, Index, 0));
    if (LPARAM(Result) = LPARAM(CB_ERR)) and ((Count = 0) or (Index < 0) or (Index > Count)) then
      Error(SListIndexError, Index);
  end;
end;

function TJvComboBoxStrings.IndexOf(const S: string): Integer;
begin
  if UseInternal then
    Result := InternalList.IndexOf(S)
  else
    Result := SendMessage(ComboBox.Handle, CB_FINDSTRINGEXACT, -1, LPARAM(PChar(S)));
end;

procedure TJvComboBoxStrings.Insert(Index: Integer; const S: string);
var
  Value: string;
begin
  if (csLoading in ComboBox.ComponentState) and UseInternal then
    InternalList.Insert(Index, S)
  else
  begin
    ComboBox.DeselectProvider;
    Value := S;
    // Comclt32.dll version 5.0 or later: If CBS_LOWERCASE or CBS_UPPERCASE is set, the Unicode version of
    // CB_ADDSTRING alters the string. If using read-only global memory, this causes the application to fail.
    if ComboBox.CharCase <> ecNormal then
      UniqueString(Value);
    if SendMessage(ComboBox.Handle, CB_INSERTSTRING, Index, LPARAM(PChar(Value))) < 0 then
      raise EOutOfResources.CreateRes(@SInsertLineError);
  end;
end;

{ Copies the strings at the combo box to the InternalList. To minimize the memory usage when a
  large list is used, each item copied is immediately removed from the combo box list. }

procedure TJvComboBoxStrings.MakeListInternal;
var
  Cnt: Integer;
  Len: Integer;
  S: string;
  Obj: TObject;
begin
  SendMessage(ComboBox.Handle, WM_SETREDRAW, Ord(False), 0);
  try
    InternalList.Clear;
    Cnt := SendMessage(ComboBox.Handle, CB_GETCOUNT, 0, 0);
    while Cnt > 0 do
    begin
      Len := SendMessage(ComboBox.Handle, CB_GETLBTEXTLEN, 0, 0);
      if Len <> CB_ERR then
      begin
        SetLength(S, Len);
        if Len > 0 then
          SendMessage(ComboBox.Handle, CB_GETLBTEXT, 0, LPARAM(PChar(S)));
        Obj := TObject(SendMessage(ComboBox.Handle, CB_GETITEMDATA, 0, 0));
        SendMessage(ComboBox.Handle, CB_DELETESTRING, 0, 0);
        InternalList.AddObject(S, Obj);
      end;
      Dec(Cnt);
    end;
  finally
    UseInternal := True;
    if not Updating then
      SendMessage(ComboBox.Handle, WM_SETREDRAW, Ord(True), 0);
  end;
end;

procedure TJvComboBoxStrings.PutObject(Index: Integer; AObject: TObject);
begin
  if UseInternal then
    InternalList.Objects[Index] := AObject
  else
    SendMessage(ComboBox.Handle, CB_SETITEMDATA, Index, LPARAM(AObject));
end;

procedure TJvComboBoxStrings.SetComboBox(Value: TJvCustomComboBox);
begin
  inherited ComboBox := Value;
end;

procedure TJvComboBoxStrings.SetUpdateState(Updating: Boolean);
begin
  FUpdating := Updating;
  SendMessage(ComboBox.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then
    ComboBox.Refresh;
end;

procedure TJvComboBoxStrings.SetWndDestroying(Destroying: Boolean);
begin
  if Destroying then
    Inc(FDestroyCnt)
  else
  if FDestroyCnt > 0 then
    Dec(FDestroyCnt);
end;

//=== { TJvCustomComboBox } ==================================================

constructor TJvCustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConsumerSvc := TJvDataConsumer.Create(Self, [DPA_RenderDisabledAsGrayed,
    DPA_ConsumerDisplaysList]);
  FConsumerSvc.OnChanged := ConsumerServiceChanged;
  FConsumerSvc.AfterCreateSubSvc := ConsumerSubServiceCreated;
  FMaxPixel := TJvMaxPixel.Create(Self);
  FMaxPixel.OnChanged := MaxPixelChanged;
  FEmptyFontColor := clGrayText;
end;

destructor TJvCustomComboBox.Destroy;
begin
  FMaxPixel.Free;
  FreeAndNil(FConsumerSvc);
  inherited Destroy;
end;

procedure TJvCustomComboBox.CNCommand(var Msg: TWMCommand);
var
  VL: IJvDataConsumerViewList;
  Item: IJvDataItem;
  ItemText: IJvDataItemText;
begin
  case Msg.NotifyCode of
    CBN_SELCHANGE:
      begin
        if IsProviderSelected then
        begin
          Provider.Enter;
          try
            if Supports(Provider as IJvDataConsumer, IJvDataConsumerViewList, VL) then
            begin
              Item := VL.Item(ItemIndex);
              if Supports(Item, IJvDataItemText, ItemText) then
                Text := ItemText.Text
              else
                Text := '';
            end
            else
            begin
              Item := nil;
              Text := '';
            end;
            Click;
            Select;
            Provider.ItemSelected(Item);
          finally
            Provider.Leave;
          end;
        end
        else
          inherited;
      end;
    else
      inherited;
  end;
end;

procedure TJvCustomComboBox.CNMeasureItem(var Msg: TWMMeasureItem);
begin
  inherited; // Normal behavior, specifically setting correct ItemHeight
  { Call MeasureItem if a provider is selected and the style is not csOwnerDrawVariable.
    if Style is set to csOwnerDrawVariable Measure will have been called already. }
  if (Style <> csOwnerDrawVariable) and IsProviderSelected then
    with Msg.MeasureItemStruct^ do
      MeasureItem(itemID, Integer(itemHeight));
end;

procedure TJvCustomComboBox.ConsumerServiceChanged(Sender: TJvDataConsumer;
  Reason: TJvDataConsumerChangeReason);
begin
  if (Reason = ccrProviderSelect) and not IsProviderSelected and not FProviderToggle then
  begin
    TJvComboBoxStrings(Items).MakeListInternal;
    FProviderIsActive := True;
    FProviderToggle := True;
    RecreateWnd;
  end
  else
  if (Reason = ccrProviderSelect) and IsProviderSelected and not FProviderToggle then
  begin
    TJvComboBoxStrings(Items).ActivateInternal; // apply internal string list to combo box
    FProviderIsActive := False;
    FProviderToggle := True;
    RecreateWnd;
  end;
  if not FProviderToggle or (Reason = ccrProviderSelect) then
  begin
    UpdateItemCount;
    Refresh;
  end;
  if FProviderToggle and (Reason = ccrProviderSelect) then
    FProviderToggle := False;
end;

procedure TJvCustomComboBox.ConsumerSubServiceCreated(Sender: TJvDataConsumer;
  SubSvc: TJvDataConsumerAggregatedObject);
var
  VL: IJvDataConsumerViewList;
begin
  if SubSvc.GetInterface(IJvDataConsumerViewList, VL) then
  begin
    VL.ExpandOnNewItem := True;
    VL.AutoExpandLevel := -1;
    VL.RebuildView;
  end;
end;

procedure TJvCustomComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if IsProviderSelected then
  begin
    Params.Style := Params.Style and not (CBS_SORT or CBS_HASSTRINGS);
    if Params.Style and (CBS_OWNERDRAWVARIABLE or CBS_OWNERDRAWFIXED) = 0 then
      Params.Style := Params.Style or CBS_OWNERDRAWFIXED;
  end;
  FIsFixedHeight := (Params.Style and CBS_OWNERDRAWVARIABLE) = 0;
end;

procedure TJvCustomComboBox.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(EditHandle, EM_SETREADONLY, Ord(ReadOnly), 0);
  if (FDropDownWidth > 0) and not (csDesigning in ComponentState) then
    SendMessage(Handle, CB_SETDROPPEDWIDTH, FDropDownWidth, 0);
  UpdateItemCount;
  if Focused then
    DoEmptyValueEnter
  else
    DoEmptyValueExit;
end;

procedure TJvCustomComboBox.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('Ctl3D', ReadCtl3D, nil, False);
  Filer.DefineProperty('ParentCtl3D', ReadParentCtl3D, nil, False);
end;

function TJvCustomComboBox.DeleteExactString(const Value: string; All: Boolean;
  CaseSensitive: Boolean): Integer;
begin
  Result := TJvItemsSearchs.DeleteExactString(Items, Value, CaseSensitive);
end;

procedure TJvCustomComboBox.DeselectProvider;
begin
  Provider.Provider := nil;
end;

procedure TJvCustomComboBox.DestroyWnd;
begin
  if IsProviderSelected then
    TJvComboBoxStrings(Items).SetWndDestroying(True);
  try
    inherited DestroyWnd;
  finally
    if IsProviderSelected then
      TJvComboBoxStrings(Items).SetWndDestroying(False);
  end;
end;

procedure TJvCustomComboBox.DoEmptyValueEnter;
begin
  if EmptyValue <> '' then
  begin
    if FIsEmptyValue then
    begin
      Text := '';
      FIsEmptyValue := False;
      if not (csDesigning in ComponentState) then
        Font.Color := FOldFontColor;
    end;
  end;
end;

procedure TJvCustomComboBox.DoEmptyValueExit;
begin
  if EmptyValue <> '' then
  begin
    if Text = '' then
    begin
      Text := EmptyValue;
      FIsEmptyValue := True;
      if not (csDesigning in ComponentState) then
      begin
        FOldFontColor := Font.Color;
        Font.Color := FEmptyFontColor;
      end;
    end;
  end;
end;

procedure TJvCustomComboBox.DoEnter;
begin
  inherited DoEnter;
  DoEmptyValueEnter;
end;

procedure TJvCustomComboBox.DoExit;
begin
  inherited DoExit;
  DoEmptyValueExit;
end;

procedure TJvCustomComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  HeightIndex: Integer;
  NewHeight: Integer;
  InvokeOrgRender: Boolean;
  VL: IJvDataConsumerViewList;
  Item: IJvDataItem;
  ItemsRenderer: IJvDataItemsRenderer;
  ItemRenderer: IJvDataItemRenderer;
  ItemText: IJvDataItemText;
  DrawState: TProviderDrawStates;
begin
  if csDestroying in ComponentState then
    Exit;
  TControlCanvas(Canvas).UpdateTextFlags;
  if (MeasureStyle = cmsBeforeDraw) and not FIsFixedHeight then
  begin
    NewHeight := FLastSetItemHeight;
    if odComboBoxEdit in State then
      HeightIndex := -1
    else
      HeightIndex := Index;
    PerformMeasureItem(HeightIndex, NewHeight);
    Perform(CB_SETITEMHEIGHT, HeightIndex, NewHeight);
  end;
  // (rom) Strange, this is already the overridden implementor of OnDrawItem
  if Assigned(OnDrawItem) and (Style in [csOwnerDrawFixed, csOwnerDrawVariable]) then
    OnDrawItem(Self, Index, Rect, State)
  else
  begin
    InvokeOrgRender := False;
    DrawState := DP_OwnerDrawStateToProviderDrawState(State);
    if not Enabled then
      DrawState := DrawState + [pdsDisabled, pdsGrayed];
    if IsProviderSelected then
    begin
      Provider.Enter;
      try
        if Supports(Provider as IJvDataConsumer, IJvDataConsumerViewList, VL) then
        begin
          Item := VL.Item(Index);
          if Item <> nil then
          begin
            Inc(Rect.Left, VL.ItemLevel(Index) * VL.LevelIndent);
            Canvas.Font := Font;
            if odSelected in State then
            begin
              Canvas.Brush.Color := clHighlight;
              Canvas.Font.Color  := clHighlightText;
            end
            else
              Canvas.Brush.Color := Color;
            Canvas.FillRect(Rect);
            if Supports(Item, IJvDataItemRenderer, ItemRenderer) then
              ItemRenderer.Draw(Canvas, Rect, DrawState)
            else
            if DP_FindItemsRenderer(Item, ItemsRenderer) then
              ItemsRenderer.DrawItem(Canvas, Rect, Item, DrawState)
            else
            if Supports(Item, IJvDataItemText, ItemText) then
              Canvas.TextRect(Rect, Rect.Left, Rect.Top, ItemText.Text)
            else
              Canvas.TextRect(Rect, Rect.Left, Rect.Top, RsDataItemRenderHasNoText);
          end
          else
            InvokeOrgRender := True;
        end
        else
          InvokeOrgRender := True;
      finally
        Provider.Leave;
      end;
    end
    else
      InvokeOrgRender := True;
    if InvokeOrgRender then
    begin
      Canvas.FillRect(Rect);
      if (Index >= 0) and (Index <= Items.Count) then
        Canvas.TextOut(Rect.Left + 2, Rect.Top, GetItemText(Index));
    end;
  end;
end;

function TJvCustomComboBox.GetItemCount: Integer;
var
  VL: IJvDataConsumerViewList;
begin
  if IsProviderSelected then
  begin
    Provider.Enter;
    try
      if Supports(Provider as IJvDataConsumer, IJvDataConsumerViewList, VL) then
        Result := VL.Count
      else
        Result := 0;
    finally
      Provider.Leave;
    end;
  end
  else
    Result := inherited GetItemCount;
end;

function TJvCustomComboBox.GetItemsClass: TCustomComboBoxStringsClass;
begin
  Result := TJvComboBoxStrings;
end;

function TJvCustomComboBox.GetItemText(Index: Integer): string;
var
  VL: IJvDataConsumerViewList;
  Item: IJvDataItem;
  ItemText: IJvDataItemText;
begin
  if IsProviderSelected then
  begin
    if Supports(Provider as IJvDataConsumer, IJvDataConsumerViewList, VL) then
    begin
      Provider.Enter;
      try
        if (Index >= 0) and (Index < VL.Count) then
        begin
          Item := VL.Item(Index);
          if Supports(Item, IJvDataItemText, ItemText) then
            Result := ItemText.Text
          else
            Result := RsDataItemRenderHasNoText;
        end
        else
          TJvComboBoxStrings(Items).Error(SListIndexError, Index);
      finally
        Provider.Leave;
      end;
    end
    else
      Result := '';
//      TJvComboBoxStrings(Items).Error(SListIndexError, Index);
  end
  else
    Result := Items[Index];
end;

function TJvCustomComboBox.GetMeasureStyle: TJvComboBoxMeasureStyle;
begin
  Result := FMeasureStyle;
end;

function TJvCustomComboBox.GetText: TCaption;
begin
  if FIsEmptyValue then
    Result := ''
  else
    Result := inherited Text;
end;

function TJvCustomComboBox.HandleFindString(StartIndex: Integer; Value: string;
  ExactMatch: Boolean): Integer;
var
  VL: IJvDataConsumerViewList;
  HasLooped: Boolean;
  Item: IJvDataItem;
  ItemText: IJvDataItemText;
begin
  if IsProviderSelected and
    Supports(Provider as IJvDataConsumer, IJvDataConsumerViewList, VL) then
  begin
    Provider.Enter;
    try
      HasLooped := False;
      Result := StartIndex + 1;
      while True do
      begin
        Item := VL.Item(Result);
        if Supports(Item, IJvDataItemText, ItemText) then
        begin
          if ExactMatch then
          begin
            if AnsiSameText(Value, ItemText.Text) then
              Break;
          end
          else
          if AnsiStrLIComp(PChar(Value), PChar(ItemText.Text), Length(Value)) = 0 then
            Break;
        end;
        Inc(Result);
        if Result >= VL.Count then
        begin
          Result := 0;
          HasLooped := True;
        end;
        if (Result > StartIndex) and HasLooped then
        begin
          Result := -1;
          Exit;
        end;
      end;
    finally
      Provider.Leave;
    end;
  end
  else
    Result := -1;
end;

function TJvCustomComboBox.IsItemHeightStored: Boolean;
var
  Value: Integer;
begin
  if Style in [csOwnerDrawFixed, csOwnerDrawVariable] then
  begin
    Value := ItemHeight;
    Result := (Value <> 16) and (Value <> 0);
  end
  else
    Result := False;
end;

function TJvCustomComboBox.IsProviderSelected: Boolean;
begin
  Result := FProviderIsActive;
end;

procedure TJvCustomComboBox.KeyPress(var Key: Char);
begin
  if ReadOnly and (AutoComplete or (Key = Chr(VK_BACK))) then
    Key := #0;
  inherited KeyPress(Key);
end;

procedure TJvCustomComboBox.Loaded;
begin
  inherited Loaded;
  RecreateWnd;      // Force measuring at the correct moment
end;

procedure TJvCustomComboBox.MaxPixelChanged(Sender: TObject);
var
  St: string;
begin
  if Style <> csDropDownList then
  begin
    St := Text;
    FMaxPixel.Test(St, Font);
    if Text <> St then
      Text := St;
    SelStart := Length(Text);
  end;
end;

procedure TJvCustomComboBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  if not (csLoading in ComponentState) and
    (MeasureStyle = cmsStandard) and not IsProviderSelected then
    PerformMeasureItem(Index, Height);
end;

procedure TJvCustomComboBox.PerformMeasure;
var
  MaxCnt: Integer;
  Index: Integer;
  NewHeight: Integer;
begin
  if FIsFixedHeight then
    MaxCnt := 0
  else
    MaxCnt := GetItemCount - 1;
  for Index := -1 to MaxCnt do
  begin
    NewHeight := FLastSetItemHeight;
    PerformMeasureItem(Index, NewHeight);
    Perform(CB_SETITEMHEIGHT, Index, NewHeight);
  end;
end;

procedure TJvCustomComboBox.PerformMeasureItem(Index: Integer; var Height: Integer);
var
  TmpSize: TSize;
  VL: IJvDataConsumerViewList;
  Item: IJvDataItem;
  ItemsRenderer: IJvDataItemsRenderer;
  ItemRenderer: IJvDataItemRenderer;
begin
  if Assigned(OnMeasureItem) and (Style in [csOwnerDrawFixed, csOwnerDrawVariable]) then
    OnMeasureItem(Self, Index, Height)
  else
  begin
    TmpSize.cy := Height;
    if IsProviderSelected then
    begin
      Provider.Enter;
      try
        if ((Index = -1) or IsFixedHeight or not HandleAllocated) and
            Supports(Provider.ProviderIntf, IJvDataItemsRenderer, ItemsRenderer) then
          TmpSize := ItemsRenderer.AvgItemSize(Canvas)
        else
        if (Index <> -1) and not IsFixedHeight and HandleAllocated then
        begin
          if Supports(Provider as IJvDataConsumer, IJvDataConsumerViewList, VL) then
          begin
            Item := VL.Item(Index);
            if Supports(Item, IJvDataItemRenderer, ItemRenderer) then
              TmpSize := ItemRenderer.Measure(Canvas)
            else
            if DP_FindItemsRenderer(Item, ItemsRenderer) then
              TmpSize := ItemsRenderer.MeasureItem(Canvas, Item);
          end;
        end;
        if TmpSize.cy > Height then
          Height := TmpSize.cy;
      finally
        Provider.Leave;
      end;
    end;
  end;
end;

procedure TJvCustomComboBox.ReadCtl3D(Reader: TReader);
begin
  Flat := not Reader.ReadBoolean;
end;

procedure TJvCustomComboBox.ReadParentCtl3D(Reader: TReader);
begin
  ParentFlat := Reader.ReadBoolean;
end;

function TJvCustomComboBox.SearchExactString(const Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := TJvItemsSearchs.SearchExactString(Items, Value, CaseSensitive);
end;

function TJvCustomComboBox.SearchPrefix(const Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := TJvItemsSearchs.SearchPrefix(Items, Value, CaseSensitive);
end;

function TJvCustomComboBox.SearchSubString(const Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := TJvItemsSearchs.SearchSubString(Items, Value, CaseSensitive);
end;

procedure TJvCustomComboBox.SetConsumerService(Value: TJvDataConsumer);
begin
end;

procedure TJvCustomComboBox.SetDropDownWidth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FDropDownWidth then
  begin
    FDropDownWidth := Value;
    if HandleAllocated and not (csDesigning in ComponentState) then
      SendMessage(Handle, CB_SETDROPPEDWIDTH, Value, 0);
  end;
end;

procedure TJvCustomComboBox.SetEmptyValue(const Value: string);
begin
  FEmptyValue := Value;
  if HandleAllocated then
    if Focused then
      DoEmptyValueEnter
    else
      DoEmptyValueExit;
end;

procedure TJvCustomComboBox.SetItemHeight(Value: Integer);
begin
  FLastSetItemHeight := Value;
  inherited SetItemHeight(Value);
end;

procedure TJvCustomComboBox.SetMeasureStyle(Value: TJvComboBoxMeasureStyle);
begin
  if Value <> MeasureStyle then
  begin
    FMeasureStyle := Value;
    RecreateWnd;
  end;
end;

function TJvCustomComboBox.GetFlat: Boolean;
begin
  Result := not Ctl3D;
end;

function TJvCustomComboBox.GetParentFlat: Boolean;
begin
  Result := ParentCtl3D;
end;

procedure TJvCustomComboBox.SetFlat(const Value: Boolean);
begin
  Ctl3D := not Value;
end;

procedure TJvCustomComboBox.SetParentFlat(const Value: Boolean);
begin
  ParentCtl3D := Value;
end;

procedure TJvCustomComboBox.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    SendMessage(EditHandle, EM_SETREADONLY, Ord(Value), 0);
  end;
end;

procedure TJvCustomComboBox.SetText(const Value: TCaption);
begin
  inherited Text := Value;
end;

procedure TJvCustomComboBox.UpdateItemCount;
var
  VL: IJvDataConsumerViewList;
  Cnt: Integer;
  EmptyChr: Char;
begin
  if HandleAllocated and IsProviderSelected and
    Supports(Provider as IJvDataConsumer, IJvDataConsumerViewList, VL) then
  begin
    Cnt := VL.Count - SendMessage(Handle, CB_GETCOUNT, 0, 0);
    EmptyChr := #0;
    while Cnt > 0 do
    begin
      SendMessage(Handle, CB_ADDSTRING, 0, LPARAM(@EmptyChr));
      Dec(Cnt);
    end;
    while Cnt < 0 do
    begin
      SendMessage(Handle, CB_DELETESTRING, 0, 0);
      Inc(Cnt);
    end;
  end;
end;

procedure TJvCustomComboBox.WMInitDialog(var Msg: TWMInitDialog);
begin
  inherited;
  if (MeasureStyle = cmsAfterCreate) or
    (IsProviderSelected and ((MeasureStyle <> cmsBeforeDraw) or FIsFixedHeight)) then
    PerformMeasure;
end;

procedure TJvCustomComboBox.WMLButtonDblClk(var Msg: TWMLButtonDblClk);
begin
  if not ReadOnly then
    inherited;
end;

procedure TJvCustomComboBox.WMLButtonDown(var Msg: TWMLButtonDown);
begin
  if ReadOnly then
    SetFocus
  else
    inherited;
end;

procedure TJvCustomComboBox.WndProc(var Msg: TMessage);
begin
  if ReadOnly and not (csDesigning in ComponentState) then
  begin
    case Msg.Msg of
      WM_KEYDOWN:
        if Integer(Msg.WParam) in [VK_DOWN, VK_UP, VK_RIGHT, VK_LEFT, VK_F4] then
        begin
          // (rom) please english comments
          // see keelab aktiivse itemi vahetamise nooleklahvidega DDL kui CB on aktiivne
          Msg.Result := 0;
          Exit;
        end;
      WM_CHAR:
        begin
          // DDL trykkides ei aktiveeriks selle tahega algavat itemit
          Msg.Result := 0;
          Exit;
        end;
      WM_SYSKEYDOWN:
        if (Msg.WParam = VK_DOWN) or (Msg.WParam = VK_UP) then
        begin
          // see keelab Ald+Down listi avamise fookuses DDL CB-l
          Msg.Result := 0;
          Exit;
        end;
      WM_COMMAND:
        // DD editis nooleklahviga vahetamise valtimiseks kui fookuses
        if HiWord(Msg.WParam) = CBN_SELCHANGE then
        begin
          Msg.Result := 0;
          Exit;
        end;
      // (rom) these values need an explanation
      WM_USER + $B900:
        if Msg.WParam = VK_F4 then
        begin
          // DD F4 ei avaks
          Msg.Result := 1;
          Exit;
        end;
      WM_USER + $B904:
        if (Msg.WParam = VK_DOWN) or (Msg.WParam = VK_UP) then
        begin
          // DD Alt+ down ei avaks
          Msg.Result := 1;
          Exit;
        end;
    end;
  end;
  if IsProviderSelected then
    case Msg.Msg of
      CB_FINDSTRING:
        begin
          Msg.Result := HandleFindString(Msg.WParam, PChar(Msg.LParam), False);
          if Msg.Result < 0 then
            Msg.Result := CB_ERR;
          Exit;
        end;
      CB_SELECTSTRING:
        begin
          Msg.Result := HandleFindString(Msg.WParam, PChar(Msg.LParam), False);
          if Msg.Result < 0 then
            Msg.Result := CB_ERR
          else
            Perform(CB_SETCURSEL, Msg.Result, 0);
          Exit;
        end;
      CB_FINDSTRINGEXACT:
        begin
          Msg.Result := HandleFindString(Msg.WParam, PChar(Msg.LParam), True);
          if Msg.Result < 0 then
            Msg.Result := CB_ERR;
          Exit;
        end;
    end;
  inherited WndProc(Msg);
end;

//=== { TJvPrivForm } ========================================================

function TJvPrivForm.GetValue: Variant;
begin
  Result := '';
end;

procedure TJvPrivForm.SetValue(const Value: Variant);
begin
  {Nothing}
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
