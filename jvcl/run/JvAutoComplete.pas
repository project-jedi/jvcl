{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAutoComplete.pas, released on 2004-09-04.

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausdaden att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvAutoComplete;

{$I jvcl.inc}

interface

uses
  Windows,
  {$IFDEF VCL}
  Messages,
  {$ENDIF VCL}
  SysUtils, Classes, Controls, StdCtrls;

type
  { TControlAutoComplete implements a autocomplete code for a controls it is a
    abstract base class. After you have created an instance of a derived class
    you must either assign the AutoCompleteEvent to the OnKeyPress event of the
    control or you must call the AutoComplete method from in a KeyPress event
    handler. }
  TJvControlAutoComplete = class(TObject)
  private
    FFilter: string;
    FLastTime, FMaxFilterTime: Cardinal;
    FListSearch: Boolean;
    FActive: Boolean;
    FOnDropDown, FOnValidateItems, FOnChange, FOnValueChange: TNotifyEvent;
  protected
    function GetText: TCaption; virtual; abstract;
    procedure SetText(const Value: TCaption); virtual; abstract;
    procedure GetEditSel(out StartPos, EndPos: Integer); virtual; abstract;
    procedure SetEditSel(StartPos, EndPos: Integer); virtual; abstract;
    procedure SetItemIndex(Index: Integer); virtual; abstract;
    function GetItemIndex: Integer; virtual; abstract;
    function FindItemPrefix(IndexStart: Integer; const Prefix: string): Integer; virtual; abstract;
    function GetItemAt(Index: Integer): string; virtual; abstract;
    {$IFDEF VCL}
    function GetEditHandle: HWND; virtual; abstract;
    {$ENDIF VCL}

    function GetActive: Boolean; virtual;
    procedure SetFilter(const Value: string);

    procedure DoDropDown; dynamic;
    procedure DoValidateItems; dynamic;
    procedure DoChange; dynamic;
    procedure DoValueChange; dynamic;
  public
    constructor Create;
    procedure AutoCompleteEvent(Sender: TObject; var Key: Char);
    procedure AutoComplete(var Key: Char); virtual;

    property ListSearch: Boolean read FListSearch write FListSearch; // no edit possible
    property MaxFilterTime: Cardinal read FMaxFilterTime write FMaxFilterTime; // only with ListSearch

    property Active: Boolean read GetActive write FActive;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnValidateItems: TNotifyEvent read FOnValidateItems write FOnValidateItems;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnValueChange: TNotifyEvent read FOnValueChange write FOnValueChange;
  end;

  TJvBaseEditListAutoComplete = class(TJvControlAutoComplete)
  private
    FEditCtrl: TCustomEdit;
    FList: TStrings;

    procedure SetEditCtrl(Value: TCustomEdit);
  protected
    function GetText: TCaption; override;
    procedure SetText(const Value: TCaption); override;
    procedure GetEditSel(out StartPos, EndPos: Integer); override;
    procedure SetEditSel(StartPos, EndPos: Integer); override;
    function FindItemPrefix(IndexStart: Integer; const Prefix: string): Integer; override;
    function GetItemAt(Index: Integer): string; override;
    {$IFDEF VCL}
    function GetEditHandle: HWND; override;
    {$ENDIF VCL}

    function GetActive: Boolean; override;

    property List: TStrings read FList write FList;
  public
    constructor Create(AEditCtrl: TCustomEdit; AList: TStrings);

    property EditCtrl: TCustomEdit read FEditCtrl write SetEditCtrl;
  end;

  { TEditListAutoComplete implements a autocomplete code for a Edit/TStrings
    pair. After you have created an instance of this class you must either
    assign the AutoCompleteEvent to the OnKeyPress event of the edit control
    or you must call the AutoComplete method from in a KeyPress event handler. }
  TJvEditListAutoComplete = class(TJvBaseEditListAutoComplete)
  private
    FOnItemIndexChange: TNotifyEvent;
    FOnValidateItemIndex: TNotifyEvent;
  public
    FItemIndex: Integer;
    function GetList: TStrings;
    procedure SetList(Value: TStrings);
    procedure SetInternalItemIndex(Value: Integer);
  protected
    procedure SetItemIndex(Index: Integer); override;
    function GetItemIndex: Integer; override;
  public
    constructor Create(AEditCtrl: TCustomEdit; AList: TStrings);
    property ItemIndex: Integer read FItemIndex write SetInternalItemIndex;
    property List: TStrings read GetList write SetList;

    property OnItemIndexChange: TNotifyEvent read FOnItemIndexChange write FOnItemIndexChange;
    property OnValidateItemIndex: TNotifyEvent read FOnValidateItemIndex write FOnValidateItemIndex;
  end;

  { TEditListBoxAutoComplete implements a autocomplete code for a Edit/ListBox
    pair. After you have created an instance of this class you must either
    assign the AutoCompleteEvent to the OnKeyPress event of the edit control
    or you must call the AutoComplete method from in a KeyPress event handler. }
  TJvEditListBoxAutoComplete = class(TJvBaseEditListAutoComplete)
  private
    FListBox: TCustomListBox;
    procedure SetListBox(Value: TCustomListBox);
  protected
    procedure SetItemIndex(Index: Integer); override;
    function GetItemIndex: Integer; override;
  public
    constructor Create(AEditCtrl: TCustomEdit; AListBox: TCustomListBox);

    property ListBox: TCustomListBox read FListBox write SetListBox;
  end;

  { TComboBoxAutoComplete implements a autocomplete code for a ComboBox.
    After you have created an instance of this class you must either assign the
    AutoCompleteEvent to the OnKeyPress event of the edit control or you must
    call the AutoComplete method from in a KeyPress event handler. }
  TJvComboBoxAutoComplete = class(TJvControlAutoComplete)
  private
    FComboBox: TCustomComboBox;

    procedure SetComboBox(Value: TCustomComboBox);
  protected
    function GetText: TCaption; override;
    procedure SetText(const Value: TCaption); override;
    procedure GetEditSel(out StartPos, EndPos: Integer); override;
    procedure SetEditSel(StartPos, EndPos: Integer); override;
    procedure SetItemIndex(Index: Integer); override;
    function GetItemIndex: Integer; override;
    function FindItemPrefix(IndexStart: Integer; const Prefix: string): Integer; override;
    function GetItemAt(Index: Integer): string; override;
    {$IFDEF VCL}
    function GetEditHandle: HWND; override;
    {$ENDIF VCL}

    function GetActive: Boolean; override;
  public
    constructor Create(AComboBox: TCustomComboBox);

    property ComboBox: TCustomComboBox read FComboBox write SetComboBox;
  end;


  
  TJvLookupAutoCompleteKind = (akListBox, akStrings);

  TJvLookupAutoComplete = class(TComponent)
  private
    FAutoComplete: TJvEditListAutoComplete;
    FListBox: TCustomListBox;
    FStrings: TStrings;
    FKind: TJvLookupAutoCompleteKind;

    FOrgKeyPress: TKeyPressEvent;

    FOnChange: TNotifyEvent;
    FOnValidateStrings: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOnValueChange: TNotifyEvent;

    function GetEdit: TCustomEdit;
    function GetItemIndex: Integer;
    function GetListSearch: Boolean;
    procedure SetEdit(Value: TCustomEdit);
    procedure SetItemIndex(Value: Integer);
    procedure SetKind(Value: TJvLookupAutoCompleteKind);
    procedure SetListBox(Value: TCustomListBox);
    procedure SetListSearch(Value: Boolean);
    procedure SetStrings(Value: TStrings);
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure EvKeyPress(Sender: TObject; var Key: Char); dynamic;

    procedure EvDropDown(Sender: TObject); dynamic;
    procedure EvValidateStrings(Sender: TObject); dynamic;
    procedure EvChange(Sender: TObject); dynamic;
    procedure EvValueChange(Sender: TObject); dynamic;
    procedure EvItemIndexChange(Sender: TObject); dynamic;
    procedure EvValidateItemIndex(Sender: TObject); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
  published
    property Active: Boolean read GetActive write SetActive default True;
    property Edit: TCustomEdit read GetEdit write SetEdit;
    property ListBox: TCustomListBox read FListBox write SetListBox;
    property Strings: TStrings read FStrings write SetStrings;
    property Kind: TJvLookupAutoCompleteKind read FKind write SetKind default akListBox;
    property ListSearch: Boolean read GetListSearch write SetListSearch default False;

    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnValidateStrings: TNotifyEvent read FOnValidateStrings write FOnValidateStrings;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnValueChange: TNotifyEvent read FOnValueChange write FOnValueChange;
  end;

implementation

uses
  JvConsts, JvJCLUtils;

//=== { TJvControlAutoComplete } =============================================

constructor TJvControlAutoComplete.Create;
begin
  inherited Create;
  FActive := True;
  FMaxFilterTime := 500;
end;

function TJvControlAutoComplete.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TJvControlAutoComplete.SetFilter(const Value: string);
begin
  FFilter := Value;
end;

procedure TJvControlAutoComplete.DoValidateItems;
begin
  if Assigned(FOnValidateItems) then
    FOnValidateItems(Self);
end;

procedure TJvControlAutoComplete.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvControlAutoComplete.DoDropDown;
begin
  if Assigned(FOnDropDown) then
    FOnDropDown(Self);
end;

procedure TJvControlAutoComplete.DoValueChange;
begin
  if Assigned(FOnValueChange) then
    FOnValueChange(Self);
end;

procedure TJvControlAutoComplete.AutoCompleteEvent(Sender: TObject; var Key: Char);
begin
  AutoComplete(Key);
end;

procedure TJvControlAutoComplete.AutoComplete(var Key: Char);

  function HasSelectedText(var StartPos, EndPos: Integer): Boolean;
  begin
    GetEditSel(StartPos, EndPos);
    Result := EndPos > StartPos;
  end;

  procedure DeleteSelectedText;
  var
    StartPos, EndPos: Integer;
    OldText: string;
  begin
    OldText := GetText;
    GetEditSel(StartPos, EndPos);
    Delete(OldText, StartPos + 1, EndPos - StartPos);
    SetItemIndex(-1);
    SetText(OldText);
    SetEditSel(StartPos, StartPos);
  end;

  function SelectItem(const AnItem: string): Boolean;
  var
    Idx: Integer;
    ValueChange: Boolean;
  begin
    if AnItem = '' then
    begin
      Result := False;
      SetItemIndex(-1);
      DoChange;
      Exit;
    end;
    Idx := FindItemPrefix(-1, AnItem);
    Result := (Idx <> -1);
    if not Result then
      Exit;
    ValueChange := Idx <> GetItemIndex;
    SetItemIndex(Idx);
    if ListSearch then
    begin
      SetItemIndex(Idx);
      FFilter := AnItem;
    end
    else
    begin
      SetText(AnItem + Copy(GetItemAt(Idx), Length(AnItem) + 1, MaxInt));
      SetEditSel(Length(AnItem), Length(GetText));
    end;
    if ValueChange then
      DoValueChange;
  end;

var
  StartPos, EndPos: Integer;
  SaveText, OldText: TCaption;
  LastByte: Integer;
  LT: Int64;
  {$IFDEF VCL}
  Msg: TMsg;
  {$ENDIF VCL}
begin
  if not Active then
    Exit;

  if ListSearch then
  begin
    LT := GetTickCount;
    if FLastTime > LT then
      LT := $100000000 + LT; // double limit.
    if LT - FLastTime >= MaxFilterTime then
      FFilter := '';
    FLastTime := GetTickCount;
  end
  else
    FFilter := GetText;

  case Key of
    Esc {VK_ESCAPE}:
      Exit;
    Tab {VK_TAB}:
      begin
        DoValidateItems;
        DoDropDown;
      end;
    BackSpace {VK_BACK}:
      begin
        DoValidateItems;
        if HasSelectedText(StartPos, EndPos) then
          DeleteSelectedText
        else
        if not ListSearch and (GetText <> '') then
        begin
          SaveText := GetText;
          LastByte := StartPos;
          {$IFDEF VCL}
          while ByteType(SaveText, LastByte) = mbTrailByte do
            Dec(LastByte);
          {$ENDIF VCL}
          OldText := Copy(SaveText, 1, LastByte - 1);
          SetItemIndex(-1);
          SetText(OldText + Copy(SaveText, EndPos + 1, MaxInt));
          SetEditSel(LastByte - 1, LastByte - 1);
          FFilter := GetText;
        end
        else
        begin
          while ByteType(FFilter, Length(FFilter)) = mbTrailByte do
            Delete(FFilter, Length(FFilter), 1);
          Delete(FFilter, Length(FFilter), 1);
        end;
        Key := #0;
        DoChange;
      end;
  else
    DoValidateItems;
    DoDropDown;

    if HasSelectedText(StartPos, EndPos) then
      SaveText := Copy(FFilter, 1, StartPos) + Key
    else
      SaveText := FFilter + Key;

    {$IFDEF VCL}
    if Key in LeadBytes then
    begin
      if PeekMessage(Msg, GetEditHandle, 0, 0, PM_NOREMOVE) and (Msg.Message = WM_CHAR) then
      begin
        if SelectItem(SaveText + Char(Msg.WParam)) then
        begin
          PeekMessage(Msg, GetEditHandle, 0, 0, PM_REMOVE);
          Key := #0;
        end;
      end;
    end
    else
    {$ENDIF VCL}
    if SelectItem(SaveText) then
      Key := #0;
  end;
end;

//=== { TJvBaseEditListAutoComplete } ========================================

constructor TJvBaseEditListAutoComplete.Create(AEditCtrl: TCustomEdit;
  AList: TStrings);
begin
  inherited Create;
  FEditCtrl := AEditCtrl;
  FList := AList;
end;

procedure TJvBaseEditListAutoComplete.SetEditCtrl(Value: TCustomEdit);
begin
  FEditCtrl := Value;
  if FEditCtrl <> nil then
    SetFilter(FEditCtrl.Text)
  else
    SetFilter('');
end;

type
  TCustomEditAccess = class(TCustomEdit);

function TJvBaseEditListAutoComplete.GetText: TCaption;
begin
  Result := EditCtrl.Text;
end;

procedure TJvBaseEditListAutoComplete.SetText(const Value: TCaption);
begin
  EditCtrl.Text := Value;
end;

procedure TJvBaseEditListAutoComplete.GetEditSel(out StartPos, EndPos: Integer);
{$IFDEF VisualCLX}
var
  MarkedText: WideString;
{$ENDIF VisualCLX}
begin
  {$IFDEF VCL}
  SendMessage(EditCtrl.Handle, EM_GETSEL, Integer(@StartPos), Integer(@EndPos));
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  StartPos := EditCtrl.SelStart;
  MarkedText := EditCtrl.SelText;
  if Copy(EditCtrl.Text, StartPos + 1, Length(MarkedText)) = MarkedText then
    EndPos := StartPos + EditCtrl.SelLength
  else
  begin
    EndPos := StartPos;
    StartPos := StartPos - EditCtrl.SelLength;
  end;
  {$ENDIF VisualCLX}
end;

procedure TJvBaseEditListAutoComplete.SetEditSel(StartPos, EndPos: Integer);
begin
  EditCtrl.SelStart := StartPos;
  EditCtrl.SelLength := EndPos - StartPos;
end;

function TJvBaseEditListAutoComplete.FindItemPrefix(IndexStart: Integer; const Prefix: string): Integer;
begin
  if List <> nil then
  begin
    for Result := IndexStart + 1 to List.Count - 1 do
      if StringStartsWith(List[Result], Prefix) then
        Exit;
    for Result := 0 to IndexStart do
      if StringStartsWith(List[Result], Prefix) then
        Exit;
  end;
  Result := -1;
end;

function TJvBaseEditListAutoComplete.GetItemAt(Index: Integer): string;
begin
  Result := List[Index];
end;

{$IFDEF VCL}
function TJvBaseEditListAutoComplete.GetEditHandle: HWND;
begin
  Result := FEditCtrl.Handle;
end;
{$ENDIF VCL}

function TJvBaseEditListAutoComplete.GetActive: Boolean;
begin
  Result := inherited GetActive and (EditCtrl <> nil) and (List <> nil) and
            not TCustomEditAccess(EditCtrl).ReadOnly;
end;

//=== { TJvEditListAutoComplete } ============================================

constructor TJvEditListAutoComplete.Create(AEditCtrl: TCustomEdit;
  AList: TStrings);
begin
  inherited Create(AEditCtrl, AList);
  FItemIndex := -1;
end;

procedure TJvEditListAutoComplete.SetInternalItemIndex(Value: Integer);
begin
  if (Value < 0) or (List = nil) then
    Value := -1;
  FItemIndex := Value;
  if (List <> nil) and (FItemIndex >= List.Count) then
    FItemIndex := List.Count - 1;
end;

function TJvEditListAutoComplete.GetList: TStrings;
begin
  Result := FList;
end;

procedure TJvEditListAutoComplete.SetList(Value: TStrings);
begin
  FItemIndex := -1;
  FList := Value;
end;

procedure TJvEditListAutoComplete.SetItemIndex(Index: Integer);
begin
  FItemIndex := Index;
  if Assigned(FOnItemIndexChange) then
    FOnItemIndexChange(Self);
end;

function TJvEditListAutoComplete.GetItemIndex: Integer;
begin
  if Assigned(FOnValidateItemIndex) then
    FOnValidateItemIndex(Self);
  Result := FItemIndex;
end;

//=== { TJvEditListBoxAutoComplete } =========================================

constructor TJvEditListBoxAutoComplete.Create(AEditCtrl: TCustomEdit; AListBox: TCustomListBox);
begin
  if AListBox = nil then
    inherited Create(AEditCtrl, nil)
  else
    inherited Create(AEditCtrl, AListBox.Items);
  FListBox := AListBox;
end;

procedure TJvEditListBoxAutoComplete.SetListBox(Value: TCustomListBox);
begin
  FListBox := Value;
  if FListBox <> nil then
    List := FListBox.Items
  else
    List := nil;
end;

procedure TJvEditListBoxAutoComplete.SetItemIndex(Index: Integer);
begin
  ListBox.ItemIndex := Index;
end;

function TJvEditListBoxAutoComplete.GetItemIndex: Integer;
begin
  Result := ListBox.ItemIndex;
end;

//=== { TJvComboBoxAutoComplete } ============================================

constructor TJvComboBoxAutoComplete.Create(AComboBox: TCustomComboBox);
begin
  inherited Create;
  FComboBox := AComboBox;
end;

type
  TCustomComboBoxAccess = class(TCustomComboBox);

function TJvComboBoxAutoComplete.GetActive: Boolean;
begin
  Result := inherited GetActive and (ComboBox <> nil);
  if ComboBox <> nil then
    FListSearch := not (TCustomComboBoxAccess(ComboBox).Style in [csDropDown {$IFDEF VCL}, csSimple{$ENDIF}]);
end;

{$IFDEF VCL}
function TJvComboBoxAutoComplete.GetEditHandle: HWND;
begin
  Result := ComboBox.Handle;
end;
{$ENDIF VCL}

procedure TJvComboBoxAutoComplete.GetEditSel(out StartPos, EndPos: Integer);
{$IFDEF VisualCLX}
var
  MarkedText: WideString;
{$ENDIF VisualCLX}
begin
  {$IFDEF VCL}
  SendMessage(ComboBox.Handle, CB_GETEDITSEL, Integer(@StartPos), Integer(@EndPos));
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  StartPos := ComboBox.SelStart;
  MarkedText := ComboBox.SelText;
  if Copy(GetText, StartPos + 1, Length(MarkedText)) = MarkedText then
    EndPos := StartPos + ComboBox.SelLength
  else
  begin
    EndPos := StartPos;
    StartPos := StartPos - ComboBox.SelLength;
  end;
  {$ENDIF VisualCLX}
end;

procedure TJvComboBoxAutoComplete.SetEditSel(StartPos, EndPos: Integer);
begin
  ComboBox.SelStart := StartPos;
  ComboBox.SelLength := EndPos - StartPos;
end;

function TJvComboBoxAutoComplete.FindItemPrefix(IndexStart: Integer;
  const Prefix: string): Integer;
begin
  for Result := IndexStart + 1 to ComboBox.Items.Count - 1 do
    if StringStartsWith(ComboBox.Items[Result], Prefix) then
      Exit;
  for Result := 0 to IndexStart do
    if StringStartsWith(ComboBox.Items[Result], Prefix) then
      Exit;
  Result := -1;
end;

procedure TJvComboBoxAutoComplete.SetItemIndex(Index: Integer);
begin
  ComboBox.ItemIndex := Index;
end;

function TJvComboBoxAutoComplete.GetItemIndex: Integer;
begin
  Result := ComboBox.ItemIndex;
end;

function TJvComboBoxAutoComplete.GetItemAt(Index: Integer): string;
begin
  Result := ComboBox.Items[Index];
end;

function TJvComboBoxAutoComplete.GetText: TCaption;
begin
  Result := TCustomComboBoxAccess(ComboBox).Text;
end;

procedure TJvComboBoxAutoComplete.SetText(const Value: TCaption);
begin
  TCustomComboBoxAccess(ComboBox).Text := Value;
end;

procedure TJvComboBoxAutoComplete.SetComboBox(Value: TCustomComboBox);
begin
  FComboBox := Value;
  if FComboBox <> nil then
    SetFilter(TCustomComboBoxAccess(FComboBox).Text)
  else
    SetFilter('');
end;

{ TJvLookupAutoComplete }

constructor TJvLookupAutoComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoComplete := TJvEditListAutoComplete.Create(nil, nil);
  FAutoComplete.OnDropDown := EvDropDown;
  FAutoComplete.OnValidateItems := EvValidateStrings;
  FAutoComplete.OnChange := EvChange;
  FAutoComplete.OnValueChange := EvValueChange;
  FAutoComplete.OnItemIndexChange := EvItemIndexChange;
  FAutoComplete.OnValidateItemIndex := EvValidateItemIndex;

  FStrings := TStringList.Create;
end;

destructor TJvLookupAutoComplete.Destroy;
begin
  SetEdit(nil); // SetEdit accesses FAutoComplete
  FAutoComplete.Free;
  SetListBox(nil);
  FStrings.Free;
  inherited Destroy;
end;

procedure TJvLookupAutoComplete.EvChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvLookupAutoComplete.EvDropDown(Sender: TObject);
begin
  if Assigned(FOnDropDown) then
    FOnDropDown(Self);
end;

procedure TJvLookupAutoComplete.EvItemIndexChange(Sender: TObject);
begin
  ItemIndex := FAutoComplete.ItemIndex;
end;

procedure TJvLookupAutoComplete.EvKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(FOrgKeyPress) then
    FOrgKeyPress(Sender, Key);
  FAutoComplete.AutoComplete(Key);
end;

procedure TJvLookupAutoComplete.EvValidateItemIndex(Sender: TObject);
begin
  FAutoComplete.ItemIndex := ItemIndex;
end;

procedure TJvLookupAutoComplete.EvValidateStrings(Sender: TObject);
begin
  if Assigned(FOnValidateStrings) then
    FOnValidateStrings(Self);
end;

procedure TJvLookupAutoComplete.EvValueChange(Sender: TObject);
begin
  if Assigned(FOnValueChange) then
    FOnValueChange(Self);
end;

function TJvLookupAutoComplete.GetActive: Boolean;
begin
  Result := FAutoComplete.Active;
end;

function TJvLookupAutoComplete.GetEdit: TCustomEdit;
begin
  Result := FAutoComplete.EditCtrl;
end;

function TJvLookupAutoComplete.GetItemIndex: Integer;
begin
  Result := -1;
  case Kind of
    akListBox:
      if ListBox <> nil then
        Result := ListBox.ItemIndex;
    akStrings:
      Result := FAutoComplete.ItemIndex;
  end;
end;

function TJvLookupAutoComplete.GetListSearch: Boolean;
begin
  Result := FAutoComplete.ListSearch;
end;

procedure TJvLookupAutoComplete.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Edit then
      Edit := nil
    else if AComponent = ListBox then
      ListBox := nil;
  end;
end;

procedure TJvLookupAutoComplete.SetActive(const Value: Boolean);
begin
  FAutoComplete.Active := Value;
end;

procedure TJvLookupAutoComplete.SetEdit(Value: TCustomEdit);
begin
  if Value <> Edit then
  begin
    if Edit <> nil then
    begin
      TCustomEditAccess(Edit).OnKeyPress := FOrgKeyPress;
      Edit.RemoveFreeNotification(Self);
    end;
    FAutoComplete.EditCtrl := Value;
    if Edit <> nil then
    begin
      Edit.FreeNotification(Self);
      FOrgKeyPress := TCustomEditAccess(Edit).OnKeyPress;
      TCustomEditAccess(Edit).OnKeyPress := EvKeyPress;
    end;
  end;
end;

procedure TJvLookupAutoComplete.SetItemIndex(Value: Integer);
begin
  case Kind of
    akListBox:
      if ListBox <> nil then
        ListBox.ItemIndex := Value;
    akStrings:
      FAutoComplete.ItemIndex := Value;
  end;
end;

procedure TJvLookupAutoComplete.SetKind(Value: TJvLookupAutoCompleteKind);
begin
  FKind := Value;
  case FKind of
    akListBox:
      if ListBox <> nil then
        FAutoComplete.List := ListBox.Items
      else
        FAutoComplete.List := nil;
    akStrings:
      FAutoComplete.List := FStrings;
  end;
end;

procedure TJvLookupAutoComplete.SetListBox(Value: TCustomListBox);
begin
  if Value <> FListBox then
  begin
    if FListBox <> nil then
      FListBox.RemoveFreeNotification(Self);
    FListBox := Value;
    if FListBox <> nil then
      FListBox.FreeNotification(Self);
    if Kind = akListBox then
    begin
      if FListBox <> nil then
        FAutoComplete.List := FListBox.Items
      else
        FAutoComplete.List := nil;
    end;
  end;
end;

procedure TJvLookupAutoComplete.SetListSearch(Value: Boolean);
begin
  FAutoComplete.ListSearch := Value;
end;

procedure TJvLookupAutoComplete.SetStrings(Value: TStrings);
begin
  if Value <> FStrings then
  begin
    FStrings.Assign(Value);
    if Kind = akStrings then
      FAutoComplete.List := FStrings;
  end;
end;

end.
