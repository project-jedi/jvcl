{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBRadioPanel.pas, released .

The Initial Developer of the Original Code is Steve Paris [paris.steve att tourisme dott gouv dott qc dott ca]
Portions created by Steve Paris are Copyright (C) 2003 Steve Paris.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Works like TDBRadioGroup except haves the look of a TPanel. Major code come
  from TDBRadioGroup.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvDBRadioPanel;

interface

uses
  Windows, 
  {$IFDEF VCL}
  Messages,
  {$ENDIF VCL}
  Classes, Controls, StdCtrls, DB, DBCtrls,
  JvComponent;

type
  TJvDBRadioPanel = class(TJvCustomPanel)
  private
    FButtons: TList;
    FItems: TStringList;
    FItemIndex: Integer;
    FColumns: Integer;
    FReading: Boolean;
    FUpdating: Boolean;
    FDataLink: TFieldDataLink;
    FValue: string;
    FValues: TStringList;
    FInSetValue: Boolean;
    FOnChange: TNotifyEvent;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    function GetItems: TStrings;
    function GetValues: TStrings;
    function GetButtonValue(Index: Integer): string;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetValue(const Value: string);
    procedure SetItems(Value: TStrings);
    procedure SetValues(Value: TStrings);
    function GetButtons(Index: Integer): TRadioButton;
    procedure ArrangeButtons;
    procedure ButtonClick(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure SetButtonCount(Value: Integer);
    procedure SetColumns(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure UpdateButtons;
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
  protected
    procedure DoBoundsChanged; override;
    procedure DoExit; override;
    procedure EnabledChanged; override;
    procedure FontChanged; override;
    procedure Change; dynamic;
    procedure Click; override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
    procedure ReadState(Reader: TReader); override;
    function CanModify: Boolean; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property DataLink: TFieldDataLink read FDataLink;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlipChildren(AllLevels: Boolean); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Buttons[Index: Integer]: TRadioButton read GetButtons;
    property Field: TField read GetField;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property Value: string read FValue write SetValue;
  published
    property Align;
    property Anchors;
    property BiDiMode;
//    property Caption;
    property BevelInner;
    property BevelOuter;
    property Color;
    property Columns: Integer read FColumns write SetColumns default 1;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Items: TStrings read GetItems write SetItems;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Values: TStrings read GetValues write SetValues;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Forms, 
  JvConsts;

//=== { TGroupButton } =======================================================

type
  TGroupButton = class(TRadioButton)
  private
    FInClick: Boolean;
    procedure CNCommand(var Msg: TWMCommand); message CN_COMMAND;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor InternalCreate(RadioGroup: TJvDBRadioPanel);
    destructor Destroy; override;
  end;

constructor TGroupButton.InternalCreate(RadioGroup: TJvDBRadioPanel);
begin
  inherited Create(RadioGroup);
  RadioGroup.FButtons.Add(Self);
  Visible := False;
  Enabled := RadioGroup.Enabled;
  ParentShowHint := False;
  OnClick := RadioGroup.ButtonClick;
  Parent := RadioGroup;
end;

destructor TGroupButton.Destroy;
begin
  TJvDBRadioPanel(Owner).FButtons.Remove(Self);
  inherited Destroy;
end;

procedure TGroupButton.CNCommand(var Msg: TWMCommand);
begin
  if not FInClick then
  begin
    FInClick := True;
    try
      if ((Msg.NotifyCode = BN_CLICKED) or
        (Msg.NotifyCode = BN_DOUBLECLICKED)) and
        TJvDBRadioPanel(Parent).CanModify then
        inherited;
    except
      Application.HandleException(Self);
    end;
    FInClick := False;
  end;
end;

procedure TGroupButton.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  TJvDBRadioPanel(Parent).KeyPress(Key);
  if Key in [Backspace, ' '] then
  begin
    if not TJvDBRadioPanel(Parent).CanModify then
      Key := #0;
  end;
end;

procedure TGroupButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  TJvDBRadioPanel(Parent).KeyDown(Key, Shift);
end;

//=== { TDBRadioPanel } ======================================================

constructor TJvDBRadioPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  ControlStyle := [csSetCaption, csDoubleClicks, csParentBackground];
  ControlStyle := [csDoubleClicks {$IFDEF COMPILER7_UP}, csParentBackground {$ENDIF}];
  FButtons := TList.Create;
  FItems := TStringList.Create;
  FItems.OnChange := ItemsChange;
  FItemIndex := -1;
  FColumns := 1;

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FValues := TStringList.Create;
end;

destructor TJvDBRadioPanel.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FValues.Free;

  SetButtonCount(0);
  FItems.OnChange := nil;
  FItems.Free;
  FButtons.Free;
  inherited Destroy;
end;

procedure TJvDBRadioPanel.ArrangeButtons;
var
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
  DeferHandle: THandle;
  ALeft: Integer;
begin
  if (FButtons.Count <> 0) and not FReading then
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    ButtonsPerCol := (FButtons.Count + FColumns - 1) div FColumns;
    ButtonWidth := (Width - 10) div FColumns;
    ButtonHeight := Height div ButtonsPerCol;
    TopMargin := 0;
    DeferHandle := BeginDeferWindowPos(FButtons.Count);
    try
      for I := 0 to FButtons.Count - 1 do
        with TGroupButton(FButtons[I]) do
        begin
          BiDiMode := Self.BiDiMode;
          ALeft := (I div ButtonsPerCol) * ButtonWidth + 8;

          if UseRightToLeftAlignment then
            ALeft := Self.ClientWidth - ALeft - ButtonWidth;

          DeferHandle := DeferWindowPos(DeferHandle, Handle, 0, ALeft,
            (I mod ButtonsPerCol) * ButtonHeight + TopMargin,
            ButtonWidth, ButtonHeight,
            SWP_NOZORDER or SWP_NOACTIVATE);
          Visible := True;
        end;
    finally
      EndDeferWindowPos(DeferHandle);
    end;
  end;
end;

procedure TJvDBRadioPanel.ButtonClick(Sender: TObject);
begin
  if not FUpdating then
  begin
    FItemIndex := FButtons.IndexOf(Sender);
    Changed;
    Click;
  end;
end;

function TJvDBRadioPanel.CanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

procedure TJvDBRadioPanel.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvDBRadioPanel.Click;
begin
  if not FInSetValue then
  begin
    inherited Click;
    if ItemIndex >= 0 then
      Value := GetButtonValue(ItemIndex);
    if FDataLink.Editing then
      FDataLink.Modified;
  end;
end;

procedure TJvDBRadioPanel.EnabledChanged;
var
  I: Integer;
begin
  inherited EnabledChanged;
  for I := 0 to FButtons.Count - 1 do
    TGroupButton(FButtons[I]).Enabled := Enabled;
end;

procedure TJvDBRadioPanel.DoExit;
begin
  try
    FDataLink.UpdateRecord;
  except
    if ItemIndex >= 0 then
      TRadioButton(Controls[ItemIndex]).SetFocus
    else
      TRadioButton(Controls[0]).SetFocus;
    raise;
  end;
  inherited DoExit;
end;

procedure TJvDBRadioPanel.FontChanged;
begin
  inherited FontChanged;
  ArrangeButtons;
end;

procedure TJvDBRadioPanel.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Integer(FDataLink);
end;

procedure TJvDBRadioPanel.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    Value := FDataLink.Field.Text
  else
    Value := '';
end;

function TJvDBRadioPanel.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (DataLink <> nil) and
    DataLink.ExecuteAction(Action);
end;

procedure TJvDBRadioPanel.FlipChildren(AllLevels: Boolean);
begin
  { The radio buttons are flipped using BiDiMode }
end;

function TJvDBRadioPanel.GetButtons(Index: Integer): TRadioButton;
begin
  Result := TRadioButton(FButtons[Index]);
end;

function TJvDBRadioPanel.GetButtonValue(Index: Integer): string;
begin
  if (Index < FValues.Count) and (FValues[Index] <> '') then
    Result := FValues[Index]
  else
  if Index < Items.Count then
    Result := Items[Index]
  else
    Result := '';
end;

procedure TJvDBRadioPanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

function TJvDBRadioPanel.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TJvDBRadioPanel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TJvDBRadioPanel.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TJvDBRadioPanel.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJvDBRadioPanel.ItemsChange(Sender: TObject);
begin
  if not FReading then
  begin
    if FItemIndex >= FItems.Count then
      FItemIndex := FItems.Count - 1;
    UpdateButtons;
  end;
end;

procedure TJvDBRadioPanel.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    Backspace, ' ':
      FDataLink.Edit;
    Esc:
      FDataLink.Reset;
  end;
end;

procedure TJvDBRadioPanel.Loaded;
begin
  inherited Loaded;
  ArrangeButtons;
end;

procedure TJvDBRadioPanel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and  (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TJvDBRadioPanel.ReadState(Reader: TReader);
begin
  FReading := True;
  inherited ReadState(Reader);
  FReading := False;
  UpdateButtons;
end;

procedure TJvDBRadioPanel.SetButtonCount(Value: Integer);
begin
  while FButtons.Count < Value do
    TGroupButton.InternalCreate(Self);
  while FButtons.Count > Value do
    TGroupButton(FButtons.Last).Free;
end;

procedure TJvDBRadioPanel.SetColumns(Value: Integer);
begin
  if Value < 1 then
    Value := 1;

  if Value > 16 then
    Value := 16;

  if FColumns <> Value then
  begin
    FColumns := Value;
    ArrangeButtons;
    Invalidate;
  end;
end;

procedure TJvDBRadioPanel.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TJvDBRadioPanel.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TJvDBRadioPanel.SetItemIndex(Value: Integer);
begin
  if FReading then
    FItemIndex := Value
  else
  begin
    if Value < -1 then
      Value := -1;

    if Value >= FButtons.Count then
      Value := FButtons.Count - 1;

    if FItemIndex <> Value then
    begin
      if FItemIndex >= 0 then
        TGroupButton(FButtons[FItemIndex]).Checked := False;
      FItemIndex := Value;
      if FItemIndex >= 0 then
        TGroupButton(FButtons[FItemIndex]).Checked := True;
    end;
  end;
end;

function TJvDBRadioPanel.GetItems: TStrings;
begin
  Result := FItems;
end;

procedure TJvDBRadioPanel.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
  DataChange(Self);
end;

procedure TJvDBRadioPanel.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

procedure TJvDBRadioPanel.SetValue(const Value: string);
var
  I, Index: Integer;
begin
  if FValue <> Value then
  begin
    FInSetValue := True;
    try
      Index := -1;
      for I := 0 to Items.Count - 1 do
        if Value = GetButtonValue(I) then
        begin
          Index := I;
          Break;
        end;
      ItemIndex := Index;
    finally
      FInSetValue := False;
    end;
    FValue := Value;
    Change;
  end;
end;

function TJvDBRadioPanel.GetValues: TStrings;
begin
  Result := FValues;
end;

procedure TJvDBRadioPanel.SetValues(Value: TStrings);
begin
  FValues.Assign(Value);
  DataChange(Self);
end;

function TJvDBRadioPanel.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (DataLink <> nil) and
    DataLink.UpdateAction(Action);
end;

procedure TJvDBRadioPanel.UpdateButtons;
var
  I: Integer;
begin
  SetButtonCount(FItems.Count);
  for I := 0 to FButtons.Count - 1 do
    TGroupButton(FButtons[I]).Caption := FItems[I];

  if FItemIndex >= 0 then
  begin
    FUpdating := True;
    TGroupButton(FButtons[FItemIndex]).Checked := True;
    FUpdating := False;
  end;
  ArrangeButtons;
  Invalidate;
end;

procedure TJvDBRadioPanel.UpdateData(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    FDataLink.Field.Text := Value;
end;

function TJvDBRadioPanel.UseRightToLeftAlignment: Boolean;
begin
  Result := inherited UseRightToLeftAlignment;
end;

procedure TJvDBRadioPanel.DoBoundsChanged;
begin
  inherited DoBoundsChanged;
  ArrangeButtons;
end;

end.

