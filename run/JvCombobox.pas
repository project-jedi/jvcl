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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvCombobox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JvDataProvider, JvDataProviderIntf, JvMaxPixel, JvItemsSearchs, 
  JvExStdCtrls;

type
  TJvCustomComboBox = class;

  { This class will be used for the Items property of the combo box.

    If a provider is active at the combo box, this list will keep the strings stored in an internal
    list.

    Whenever an item is added to the list the provider will be deactivated and the list will be
    handled by the combo box as usual. }
  TJvComboBoxStrings = class({$IFDEF COMPILER6_UP} TCustomComboBoxStrings {$ELSE} TStrings {$ENDIF})
  private
    {$IFDEF COMPILER5}
    FComboBox: TJvCustomComboBox;
    {$ENDIF COMPILER5}
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
    constructor Create;
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
    {$IFDEF COMPILER5}
    FAutoComplete: Boolean;
    FLastTime: Cardinal;      // SPM - Ported backward from Delphi 7
    FFilter: string;          // SPM - ditto
    FIsDropping: Boolean;
    FOnSelect: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    {$ENDIF COMPILER5}
    FKey: Word;
    FSearching: Boolean;
    FMaxPixel: TJvMaxPixel;
    FReadOnly: Boolean; // ain
    FConsumerSvc: TJvDataConsumer;
    FProviderIsActive: Boolean;
    FProviderToggle: Boolean;
    FIsFixedHeight: Boolean;
    FMeasureStyle: TJvComboBoxMeasureStyle;
    FLastSetItemHeight: Integer;
    {$IFDEF VCL}
    FEmptyValue: string;
    FIsEmptyValue: boolean;
    FEmptyFontColor, FOldFontColor: TColor;
    procedure SetEmptyValue(const Value: string);
    {$ENDIF VCL}
    procedure MaxPixelChanged(Sender: TObject);
    procedure SetReadOnly(const Value: Boolean); // ain
    procedure CNCommand(var Msg: TWMCommand); message CN_COMMAND;
    procedure CNMeasureItem(var Msg: TWMMeasureItem); message CN_MEASUREITEM;
    procedure WMInitDialog(var Msg: TWMInitDialog); message WM_INITDIALOG;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN; // ain
    procedure WMLButtonDblClk(var Msg: TWMLButtonDblClk); message WM_LBUTTONDBLCLK; // ain
  protected
    {$IFDEF VCL}
    function GetText: TCaption; virtual;
    procedure SetText(const Value: TCaption); virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoEmptyValueEnter; virtual;
    procedure DoEmptyValueExit; virtual;
    {$ENDIF VCL}
    procedure CreateWnd; override; // ain
    {$IFDEF COMPILER6_UP}
    function GetItemsClass: TCustomComboBoxStringsClass; override;
    {$ENDIF COMPILER6_UP}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$IFDEF COMPILER5}
    procedure KeyPress(var Key: Char); override;  // SPM - Ported backward from D7
    {$ENDIF COMPILER5}
    procedure SetItemHeight(Value: Integer); {$IFDEF COMPILER6_UP} override; {$ENDIF}
    function GetMeasureStyle: TJvComboBoxMeasureStyle;
    procedure SetMeasureStyle(Value: TJvComboBoxMeasureStyle);
    procedure PerformMeasure;
    procedure PerformMeasureItem(Index: Integer; var Height: Integer); virtual;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    {$IFDEF COMPILER5}
    function SelectItem(const AnItem: string): Boolean;  // SPM - Ported from D7
    procedure CloseUp; dynamic;
    procedure Select; dynamic;
    {$ENDIF COMPILER5}
    procedure SetConsumerService(Value: TJvDataConsumer);
    procedure ConsumerServiceChanged(Sender: TJvDataConsumer; Reason: TJvDataConsumerChangeReason);
    procedure ConsumerSubServiceCreated(Sender: TJvDataConsumer; SubSvc: TJvDataConsumerAggregatedObject);
    function IsProviderSelected: Boolean;
    procedure DeselectProvider;
    procedure UpdateItemCount;
    function HandleFindString(StartIndex: Integer; Value: string; ExactMatch: Boolean): Integer;
    procedure Loaded; override;
    property Provider: TJvDataConsumer read FConsumerSvc write SetConsumerService;
    {$IFDEF COMPILER5}
    property IsDropping: Boolean read FIsDropping write FIsDropping;
    property ItemHeight write SetItemHeight;
    {$ENDIF COMPILER5}
    property IsFixedHeight: Boolean read FIsFixedHeight;
    property MeasureStyle: TJvComboBoxMeasureStyle read GetMeasureStyle write SetMeasureStyle
      default cmsStandard;
    {$IFDEF COMPILER5}
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property AutoComplete: Boolean read FAutoComplete write FAutoComplete default True;
    {$ENDIF COMPILER5}
    property MaxPixel: TJvMaxPixel read FMaxPixel write FMaxPixel;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False; // ain
    {$IFDEF VCL}
    property Text:TCaption read GetText write SetText;
    property EmptyValue:string read FEmptyValue write SetEmptyValue;
    property EmptyFontColor:TColor read FEmptyFontColor write FEmptyFontColor default clGrayText;
    {$ENDIF VCL}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DestroyWnd; override;
    procedure WndProc(var Msg: TMessage); override; // ain
    function GetItemCount: Integer; {$IFDEF COMPILER6_UP} override; {$ELSE} virtual; {$ENDIF}
    function GetItemText(Index: Integer): string; virtual;
    function SearchExactString(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchPrefix(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchSubString(Value: string; CaseSensitive: Boolean = True): Integer;
    function DeleteExactString(Value: string; All: Boolean; CaseSensitive: Boolean = True): Integer;
  end;

  TJvComboBox = class(TJvCustomComboBox)
  published
    property HintColor;
    property MaxPixel;
    property AutoComplete default True;
    {$IFDEF COMPILER6_UP}
    property AutoDropDown default False;
    {$ENDIF COMPILER6_UP}
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
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property MeasureStyle;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property Provider;
    property PopupMenu;
    property ReadOnly; // ain
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;

implementation

uses
  Consts, TypInfo,
  {$IFDEF COMPILER6_UP}
  RTLConsts,
  {$ENDIF COMPILER6_UP}
  JvConsts, JvResources;

//=== TJvComboBoxStrings =====================================================

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

function TJvComboBoxStrings.GetInternalList: TStrings;
begin
  Result := FInternalList;
end;

function TJvComboBoxStrings.Get(Index: Integer): string;
var
  Text: array [0..4095] of Char;
  Len: Integer;
begin
  if UseInternal then
    Result := InternalList[Index]
  else
  begin
    Len := SendMessage(ComboBox.Handle, CB_GETLBTEXT, Index, Longint(@Text));
    if Len = CB_ERR then //Len := 0;
      Error(SListIndexError, Index);
    SetString(Result, Text, Len);
  end;
end;

function TJvComboBoxStrings.GetCount: Integer;
begin
  if (DestroyCount > 0) and UseInternal then
    Result := 0
  else
  begin
    if UseInternal then
    begin
      {$IFDEF COMPILER5}
      if not ComboBox.IsDropping then
      {$ENDIF COMPILER5}
        Result := InternalList.Count
      {$IFDEF COMPILER5}
      else
        Result := SendMessage(ComboBox.Handle, CB_GETCOUNT, 0, 0)
      {$ENDIF COMPILER5}
    end
    else
      Result := SendMessage(ComboBox.Handle, CB_GETCOUNT, 0, 0);
  end;
end;

function TJvComboBoxStrings.GetObject(Index: Integer): TObject;
begin
  if UseInternal then
    Result := InternalList.Objects[Index]
  else
  begin
    Result := TObject(SendMessage(ComboBox.Handle, CB_GETITEMDATA, Index, 0));
    if Longint(Result) = CB_ERR then
      Error(SListIndexError, Index);
  end;
end;

procedure TJvComboBoxStrings.PutObject(Index: Integer; AObject: TObject);
begin
  if UseInternal then
    InternalList.Objects[Index] := AObject
  else
    SendMessage(ComboBox.Handle, CB_SETITEMDATA, Index, Longint(AObject));
end;

procedure TJvComboBoxStrings.SetUpdateState(Updating: Boolean);
begin
  FUpdating := Updating;
  SendMessage(ComboBox.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then ComboBox.Refresh;
end;

procedure TJvComboBoxStrings.SetWndDestroying(Destroying: Boolean);
begin
  if Destroying then
    Inc(FDestroyCnt)
  else
  if FDestroyCnt > 0 then
    Dec(FDestroyCnt);
end;

function TJvComboBoxStrings.GetComboBox: TJvCustomComboBox;
begin
  {$IFDEF COMPILER6_UP}
  Result := TJvCustomComboBox(inherited ComboBox);
  {$ELSE}
  Result := FComboBox;
  {$ENDIF COMPILER6_UP}
end;

procedure TJvComboBoxStrings.SetComboBox(Value: TJvCustomComboBox);
begin
  {$IFDEF COMPILER6_UP}
  inherited ComboBox := Value;
  {$ELSE}
  FComboBox := Value;
  {$ENDIF COMPILER6_UP}
end;

function TJvComboBoxStrings.Add(const S: string): Integer;
begin
  if (csLoading in ComboBox.ComponentState) and UseInternal then
    Result := InternalList.Add(S)
  else
  begin
    ComboBox.DeselectProvider;
    Result := SendMessage(ComboBox.Handle, CB_ADDSTRING, 0, Longint(PChar(S)));
    if Result < 0 then
      raise EOutOfResources.Create(SInsertLineError);
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

function TJvComboBoxStrings.IndexOf(const S: string): Integer;
begin
  if UseInternal then
    Result := InternalList.IndexOf(S)
  else
    Result := SendMessage(ComboBox.Handle, CB_FINDSTRINGEXACT, -1, LongInt(PChar(S)));
end;

procedure TJvComboBoxStrings.Insert(Index: Integer; const S: string);
begin
  if (csLoading in ComboBox.ComponentState) and UseInternal then
    InternalList.Insert(Index, S)
  else
  begin
    ComboBox.DeselectProvider;
    if SendMessage(ComboBox.Handle, CB_INSERTSTRING, Index, Longint(PChar(S))) < 0 then
      raise EOutOfResources.Create(SInsertLineError);
  end;
end;

{ Copies the strings at the combo box to the InternalList. To minimize the memory usage when a
  large list is used, each item copied is immediately removed from the combo box list. }

procedure TJvComboBoxStrings.MakeListInternal;
var
  Cnt: Integer;
  Text: array [0..4095] of Char;
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
      Len := SendMessage(ComboBox.Handle, CB_GETLBTEXT, 0, Longint(@Text));
      SetString(S, Text, Len);
      Obj := TObject(SendMessage(ComboBox.Handle, CB_GETITEMDATA, 0, 0));
      SendMessage(ComboBox.Handle, CB_DELETESTRING, 0, 0);
      InternalList.AddObject(S, Obj);
      Dec(Cnt);
    end;
  finally
    UseInternal := True;
    if not Updating then
      SendMessage(ComboBox.Handle, WM_SETREDRAW, Ord(True), 0);
  end;
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
        Index := SendMessage(ComboBox.Handle, CB_ADDSTRING, 0, Longint(PChar(S)));
        if Index < 0 then
          raise EOutOfResources.Create(SInsertLineError);
        SendMessage(ComboBox.Handle, CB_SETITEMDATA, Index, Longint(Obj));
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

//=== TJvCustomComboBox ======================================================

type
  PStrings = ^TStrings;

constructor TJvCustomComboBox.Create(AOwner: TComponent);
{.$IFNDEF COMPILER7_UP}
var
  PI: PPropInfo;
  PStringsAddr: PStrings;
{.$ENDIF COMPILER7_UP}
begin
  inherited Create(AOwner);
  FConsumerSvc := TJvDataConsumer.Create(Self, [DPA_RenderDisabledAsGrayed,
    DPA_ConsumerDisplaysList]);
  FConsumerSvc.OnChanged := ConsumerServiceChanged;
  FConsumerSvc.AfterCreateSubSvc := ConsumerSubServiceCreated;
  {.$IFNDEF COMPILER7_UP}
  { The following hack assumes that TJvComboBox.Items reads directly from the private FItems field
    of TCustomComboBox and that TJvComboBox.Items is actually published.

    What we do here is remove the original string list used and place our own version in it's place.
    This would give us the benefit of keeping the list of strings (and objects) even if a provider
    is active and the combo box windows has no strings at all. }
  PI := GetPropInfo(TJvComboBox, 'Items');
  PStringsAddr := Pointer(Integer(PI.GetProc) and $00FFFFFF + Integer(Self));
  Items.Free;                                 // remove original item list (TComboBoxStrings instance)
  PStringsAddr^ := TJvComboBoxStrings.Create; // create our own implementation and put it in place.
  TJvComboBoxStrings(Items).ComboBox := Self; // link it to the combo box.
  {.$ENDIF COMPILER7_UP}
  {$IFDEF COMPILER5}
  FAutoComplete := True;
  FLastTime := 0;           // SPM - Ported backward from Delphi 7
  {$ENDIF COMPILER5}
  FSearching := False;
  FMaxPixel := TJvMaxPixel.Create(Self);
  FMaxPixel.OnChanged := MaxPixelChanged;
  FReadOnly := False; // ain
  FEmptyFontColor := clGrayText;
end;

destructor TJvCustomComboBox.Destroy;
begin
  FMaxPixel.Free;
  FreeAndNil(FConsumerSvc);
  inherited Destroy;
end;

function TJvCustomComboBox.SearchExactString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := TJvItemsSearchs.SearchExactString(Items, Value, CaseSensitive);
end;

function TJvCustomComboBox.SearchPrefix(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := TJvItemsSearchs.SearchPrefix(Items, Value, CaseSensitive);
end;

{$IFDEF COMPILER6_UP}
function TJvCustomComboBox.GetItemsClass: TCustomComboBoxStringsClass;
begin
  Result := TJvComboBoxStrings;
end;
{$ENDIF COMPILER6_UP}

procedure TJvCustomComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  FKey := Key;
end;

procedure TJvCustomComboBox.SetItemHeight(Value: Integer);
begin
  FLastSetItemHeight := Value;
  {$IFDEF COMPILER6_UP}
  inherited SetItemHeight(Value);
  {$ELSE}
  inherited ItemHeight := Value;
  {$ENDIF COMPILER6_UP}
end;

function TJvCustomComboBox.GetMeasureStyle: TJvComboBoxMeasureStyle;
begin
  Result := FMeasureStyle;
end;

procedure TJvCustomComboBox.SetMeasureStyle(Value: TJvComboBoxMeasureStyle);
begin
  if Value <> MeasureStyle then
  begin
    FMeasureStyle := Value;
    RecreateWnd;
  end;
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
    if (odComboBoxEdit in State) then
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
              Canvas.TextRect(Rect, Rect.Left, Rect.Top, ItemText.Caption)
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

procedure TJvCustomComboBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  if not (csLoading in ComponentState) and (MeasureStyle = cmsStandard) and
      not IsProviderSelected then
    PerformMeasureItem(Index, Height);
end;

{$IFDEF COMPILER5}

// SPM - Ported backward from Delphi 7 and modified:
procedure TJvCustomComboBox.KeyPress(var Key: Char);

  function HasSelectedText(var StartPos, EndPos: DWORD): Boolean;
  begin
    SendMessage(Handle, CB_GETEDITSEL, Integer(@StartPos), Integer(@EndPos));
    Result := EndPos > StartPos;
  end;

  procedure DeleteSelectedText;
  var
    StartPos, EndPos: DWORD;
    OldText: String;
  begin
    OldText := Text;
    SendMessage(Handle, CB_GETEDITSEL, Integer(@StartPos), Integer(@EndPos));
    Delete(OldText, StartPos + 1, EndPos - StartPos);
    SendMessage(Handle, CB_SETCURSEL, -1, 0);
    Text := OldText;
    SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(StartPos, StartPos));
  end;

var
  StartPos: DWORD;
  EndPos: DWORD;
  OldText: string;
  SaveText: string;
  Msg: TMsg;
  LastByte: Integer;
begin
  inherited KeyPress(Key);
  if not AutoComplete then
    Exit;
  if Style in [csDropDown, csSimple] then
    FFilter := Text
  else
  begin
   if GetTickCount - FLastTime >= 500 then
      FFilter := '';
    FLastTime := GetTickCount;
  end;
  case Ord(Key) of
    VK_ESCAPE:
      Exit;
    VK_BACK:
      begin
        if HasSelectedText(StartPos, EndPos) then
          DeleteSelectedText
        else
        if (Style in [csDropDown, csSimple]) and (Length(Text) > 0) then
        begin
          SaveText := Text;
          LastByte := StartPos;
          while ByteType(SaveText, LastByte) = mbTrailByte do
            Dec(LastByte);
          OldText := Copy(SaveText, 1, LastByte - 1);
          SendMessage(Handle, CB_SETCURSEL, -1, 0);
          Text := OldText + Copy(SaveText, EndPos + 1, MaxInt);
          SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(LastByte - 1, LastByte - 1));
          FFilter := Text;
        end
        else
        begin
          while ByteType(FFilter, Length(FFilter)) = mbTrailByte do
            Delete(FFilter, Length(FFilter), 1);
          Delete(FFilter, Length(FFilter), 1);
        end;
        Key := #0;
        Change;
      end;
  else
    if HasSelectedText(StartPos, EndPos) then
      SaveText := Copy(FFilter, 1, StartPos) + Key
    else
      SaveText := FFilter + Key;

    if Key in LeadBytes then
    begin
      if PeekMessage(Msg, Handle, 0, 0, PM_NOREMOVE) and (Msg.Message = WM_CHAR) then
      begin
        if SelectItem(SaveText + Char(Msg.WParam)) then
        begin
          PeekMessage(Msg, Handle, 0, 0, PM_REMOVE);
          Key := #0;
        end;
      end;
    end
    else
      if SelectItem(SaveText) then
        Key := #0;
  end;
end;

// SPM - Ported backward from Delphi 7 and modified:
function TJvCustomComboBox.SelectItem(const AnItem: String): Boolean;
var
  Idx: Integer;
  ValueChange: Boolean;
begin
  if Length(AnItem) = 0 then
  begin
    Result := False;
    ItemIndex := -1;
    Change;
    Exit;
  end;
  Idx := SendMessage(Handle, CB_FINDSTRING, -1, LongInt(PChar(AnItem)));
  Result := (Idx <> CB_ERR);
  if not Result then
    Exit;
  ValueChange := Idx <> ItemIndex;
  SendMessage(Handle, CB_SETCURSEL, Idx, 0);
  if (Style in [csDropDown, csSimple]) then
  begin
    Text := AnItem + Copy(GetItemText(Idx), Length(AnItem) + 1, MaxInt);
    SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Length(AnItem), Length(Text)));
  end
  else
  begin
    ItemIndex := Idx;
    FFilter := AnItem;
  end;
  if ValueChange then
  begin
    Click;
    Select;
  end;
end;

procedure TJvCustomComboBox.Select;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self)
  else
    Change;
end;

procedure TJvCustomComboBox.CloseUp;
begin
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self);
end;

{$ENDIF COMPILER5}

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
    {$IFDEF COMPILER6_UP}
    Result := inherited GetItemCount;
    {$ELSE}
    Result := Items.Count;
    {$ENDIF COMPILER6_UP}
end;

procedure TJvCustomComboBox.SetConsumerService(Value: TJvDataConsumer);
begin
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

function TJvCustomComboBox.IsProviderSelected: Boolean;
begin
  Result := FProviderIsActive;
end;

procedure TJvCustomComboBox.DeselectProvider;
begin
  Provider.Provider := nil;
end;

procedure TJvCustomComboBox.UpdateItemCount;
var
  VL: IJvDataConsumerViewList;
  Cnt: Integer;
begin
  if HandleAllocated and IsProviderSelected and
    Supports(Provider as IJvDataConsumer, IJvDataConsumerViewList, VL) then
  begin
    Cnt := VL.Count - SendMessage(Handle, CB_GETCOUNT, 0, 0);
    while Cnt > 0 do
    begin
      SendMessage(Handle, CB_ADDSTRING, 0, Integer(PChar(EmptyStr)));
      Dec(Cnt);
    end;
    while Cnt < 0 do
    begin
      SendMessage(Handle, CB_DELETESTRING, 0, 0);
      Inc(Cnt);
    end;
  end;
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
      HasLooped := False;;
      Result := StartIndex + 1;
      while True do
      begin
        Item := VL.Item(Result);
        if Supports(Item, IJvDataItemText, ItemText) then
        begin
          if ExactMatch then
          begin
            if AnsiSameText(Value, ItemText.Caption) then
              Break;
          end
          else
            if AnsiStrLIComp(PChar(Value), PChar(ItemText.Caption), Length(Value)) = 0 then
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

procedure TJvCustomComboBox.Loaded;
begin
  inherited Loaded;
  RecreateWnd;      // Force measuring at the correct moment
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
            Result := ItemText.Caption
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

function TJvCustomComboBox.SearchSubString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := TJvItemsSearchs.SearchSubString(Items, Value, CaseSensitive);
end;

function TJvCustomComboBox.DeleteExactString(Value: string; All: Boolean;
  CaseSensitive: Boolean): Integer;
begin
  Result := TJvItemsSearchs.DeleteExactString(Items, Value, CaseSensitive);
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

procedure TJvCustomComboBox.CNCommand(var Msg: TWMCommand);
var
  VL: IJvDataConsumerViewList;
  Item: IJvDataItem;
  ItemText: IJvDataItemText;
begin
  {$IFDEF COMPILER5}
  if Msg.NotifyCode = CBN_DROPDOWN then
    FIsDropping := True;
  try
  {$ENDIF COMPILER5}
    if (Msg.NotifyCode = CBN_SELCHANGE) and IsProviderSelected then
    begin
      Provider.Enter;
      try
        if Supports(Provider as IJvDataConsumer, IJvDataConsumerViewList, VL) then
        begin
          Item := VL.Item(ItemIndex);
          if Supports(Item, IJvDataItemText, ItemText) then
            Text := ItemText.Caption
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
  {$IFDEF COMPILER5}
  finally
    if Msg.NotifyCode = CBN_DROPDOWN then
      FIsDropping := False;
  end;
  {$ENDIF COMPILER5}
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

procedure TJvCustomComboBox.WMInitDialog(var Msg: TWMInitDialog);
begin
  inherited;
  if (MeasureStyle = cmsAfterCreate) or (IsProviderSelected and
      ((MeasureStyle <> cmsBeforeDraw) or FIsFixedHeight)) then
    PerformMeasure;
end;

procedure TJvCustomComboBox.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    SendMessage(EditHandle, EM_SETREADONLY, Ord(Value), 0);
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
  UpdateItemCount;
  if Focused then
    DoEmptyValueEnter
  else
    DoEmptyValueExit;
end;

{$IFDEF VCL}

procedure TJvCustomComboBox.SetEmptyValue(const Value: string);
begin
  FEmptyValue := Value;
  if HandleAllocated then
  begin
    if Focused then
      DoEmptyValueEnter
    else
      DoEmptyValueExit;
  end;
end;

procedure TJvCustomComboBox.DoEnter;
begin
  inherited;
  DoEmptyValueEnter;
end;

procedure TJvCustomComboBox.DoExit;
begin
  inherited;
  DoEmptyValueExit;
end;

procedure TJvCustomComboBox.DoEmptyValueEnter;
begin
  if EmptyValue <> '' then
  begin
    if FIsEmptyValue then
    begin
      Text := '';
      FIsEmptyValue := false;
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
      FIsEmptyValue := true;
      if not (csDesigning in ComponentState) then
      begin
        FOldFontColor := Font.Color;
        Font.Color := FEmptyFontColor;
      end;
    end;
  end;
end;

function TJvCustomComboBox.GetText: TCaption;
begin
  if FIsEmptyValue then
    Result := ''
  else
    Result := inherited Text;
end;

procedure TJvCustomComboBox.SetText(const Value: TCaption);
begin
  inherited Text := Value;
end;
{$ENDIF VCL}

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

procedure TJvCustomComboBox.WndProc(var Msg: TMessage);
begin
  if ReadOnly and not (csDesigning in ComponentState) then
  begin
    case Msg.Msg of
      WM_KEYDOWN:
        begin
          if Msg.WParam in [VK_DOWN, VK_UP, VK_RIGHT, VK_LEFT, VK_F4] then
          begin
            // see keelab aktiivse itemi vahetamise nooleklahvidega DDL kui CB on aktiivne
            Msg.Result := 0;
            Exit;
          end;
        end;
      WM_CHAR:
        begin
          // DDL trykkides ei aktiveeriks selle tahega algavat itemit
          Msg.Result := 0;
          Exit;
        end;
      WM_SYSKEYDOWN:
        begin
          if (Msg.WParam = VK_DOWN) or (Msg.WParam = VK_UP) then
          begin
            // see keelab Ald+Down listi avamise fookuses DDL CB-l
            Msg.Result := 0;
            Exit;
          end;
        end;
      WM_COMMAND:
        begin
          // DD editis nooleklahviga vahetamise valtimiseks kui fookuses
          if HiWord(Msg.WParam) = CBN_SELCHANGE then
          begin
            Msg.Result := 0;
            Exit;
          end;
        end;
      WM_USER + $B900:
        begin
          if Msg.WParam = VK_F4 then
          begin
            // DD F4 ei avaks
            Msg.Result := 1;
            Exit;
          end;
        end;
      WM_USER + $B904:
        begin
          if (Msg.WParam = VK_DOWN) or (Msg.WParam = VK_UP) then
          begin
            // DD Alt+ down ei avaks
            Msg.Result := 1;
            Exit;
          end;
        end;
    end;
  end;
  if IsProviderSelected then
    case Msg.Msg of
      CB_FINDSTRING:
        begin
          Msg.Result := HandleFindString(Msg.wParam, PChar(Msg.lParam), False);
          if Msg.Result < 0 then
            Msg.Result := CB_ERR;
          Exit;
        end;
      CB_SELECTSTRING:
        begin
          Msg.Result := HandleFindString(Msg.wParam, PChar(Msg.lParam), False);
          if Msg.Result < 0 then
            Msg.Result := CB_ERR
          else
            Perform(CB_SETCURSEL, Msg.Result, 0);
          Exit;
        end;
      CB_FINDSTRINGEXACT:
        begin
          Msg.Result := HandleFindString(Msg.wParam, PChar(Msg.lParam), True);
          if Msg.Result < 0 then
            Msg.Result := CB_ERR;
          Exit;
        end;
    end;
  inherited WndProc(Msg);
end;

procedure TJvCustomComboBox.WMLButtonDown(var Msg: TWMLButtonDown);
begin
  if ReadOnly then
    SetFocus
  else
    inherited;
end;

procedure TJvCustomComboBox.WMLButtonDblClk(var Msg: TWMLButtonDblClk);
begin
  if not ReadOnly then
    inherited;
end;


end.

