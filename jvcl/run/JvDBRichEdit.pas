{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBRichEd.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvDBRichEdit;

interface

uses
  Windows, Messages, ComCtrls, RichEdit, SysUtils, Classes,
  Graphics, Controls, Menus, StdCtrls, DB, DBCtrls,
  JvRichEdit;

type
  TJvDBRichEdit = class(TJvCustomRichEdit)
  private
    FDataLink: TFieldDataLink;
    FUpdating: Boolean;
    FStateChanging: Boolean;
    FMemoLoaded: Boolean;
    FAutoDisplay: Boolean;
    FFocused: Boolean;
    FDataSave: string;
    FBeepOnError: Boolean;
    function GetField: TField;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetAutoDisplay(Value: Boolean);
    procedure SetFocused(Value: Boolean);
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
    procedure WMLButtonDblClk(var Msg: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure EMSetCharFormat(var Msg: TMessage); message EM_SETCHARFORMAT;
    procedure EMSetParaFormat(var Msg: TMessage); message EM_SETPARAFORMAT;
  protected
    procedure DoClipboardPaste; override;
    procedure DoClipboardCut; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Change; override;
    function EditCanModify: Boolean; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure SetPlainText(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadMemo; virtual;
    procedure UpdateMemo;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property Lines;
  published
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default True;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property AdvancedTypography;
    property Align;
    property Alignment;
    property AllowObjects;
    property AllowInPlace;
    property AutoAdvancedTypography;
    property AutoURLDetect;
    property AutoVerbMenu;
    property BorderStyle;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HideScrollBars;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property LangOptions;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PlainText;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ScrollBars;
    property ShowHint;
    property SelectionBar;
    property StreamFormat;
    property StreamMode;
    property TabOrder;
    property TabStop default True;
    property Title;
    property UndoLimit;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordSelection;
    property WordWrap;
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
    property OnResizeRequest;
    property OnSelectionChange;
    property OnProtectChange; { obsolete }
    property OnProtectChangeEx;
    property OnSaveClipboard;
    property OnStartDrag;
    property OnContextPopup;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnEndDock;
    property OnStartDock;
    property OnTextNotFound;
    property OnCloseFindDialog;
    property OnURLClick;
  end;

implementation

constructor TJvDBRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  FAutoDisplay := True;
  FBeepOnError := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TJvDBRichEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TJvDBRichEdit.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

procedure TJvDBRichEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;

function TJvDBRichEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TJvDBRichEdit.EditCanModify: Boolean;
begin
  FStateChanging := True;
  try
    Result := FDataLink.Editing;
    if not Result and Assigned(FDataLink.Field) then
    try
      if FDataLink.Field.IsBlob then
        FDataSave := FDataLink.Field.AsString;
      Result := FDataLink.Edit;
    finally
      FDataSave := '';
    end;
  finally
    FStateChanging := False;
  end;
end;

procedure TJvDBRichEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if FMemoLoaded then
  begin
    if (Key in [VK_DELETE, VK_BACK, VK_CLEAR]) or
      ((Key = VK_INSERT) and (ssShift in Shift)) or
      (((Key = Ord('V')) or (Key = Ord('X'))) and (ssCtrl in Shift)) then
      EditCanModify;
  end
  else
    Key := 0;
end;

procedure TJvDBRichEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if FMemoLoaded then
  begin
    if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
      not FDataLink.Field.IsValidChar(Key) then
    begin
      if BeepOnError then
        Beep;
      Key := #0;
    end;
    case Key of
      ^H, ^I, ^J, ^M, ^V, ^X, #32..#255: EditCanModify;
      #27: FDataLink.Reset;
    end;
  end
  else
  begin
    if Key = Chr(VK_RETURN) then
      LoadMemo;
    if FMemoLoaded then
      Key := #0;
  end;
end;

procedure TJvDBRichEdit.Change;
begin
  if FMemoLoaded then
    FDataLink.Modified;
  FMemoLoaded := True;
  inherited Change;
end;

function TJvDBRichEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBRichEdit.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

function TJvDBRichEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBRichEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TJvDBRichEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJvDBRichEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TJvDBRichEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJvDBRichEdit.LoadMemo;
begin
  if FMemoLoaded or (FDataLink.Field = nil) or not FDataLink.Field.IsBlob then
    Exit;
  FUpdating := True;
  try
    try
      Lines.Assign(FDataLink.Field);
      FMemoLoaded := True;
    except
      on E: EOutOfResources do
        Lines.Text := Format('(%s)', [E.Message]);
    end;
    EditingChange(Self);
  finally
    FUpdating := False;
  end;
end;

procedure TJvDBRichEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field = nil then
  begin
    if csDesigning in ComponentState then
      Text := Name
    else
      Text := '';
    FMemoLoaded := False;
  end
  else
  if FDataLink.Field.IsBlob then
  begin
    if AutoDisplay or (FDataLink.Editing and FMemoLoaded) then
    begin
      { Check if the data has changed since we read it the first time }
      if FStateChanging and (FDataSave <> '') and
        (FDataSave = FDataLink.Field.AsString) then
        Exit;
      FMemoLoaded := False;
      LoadMemo;
    end
    else
    begin
      Text := Format('(%s)', [FDataLink.Field.DisplayLabel]);
      FMemoLoaded := False;
    end;
  end
  else
  if FDataLink.CanModify then
  begin
    if not FStateChanging then
    begin
      inherited SetPlainText(True);
      if FFocused then
        Text := FDataLink.Field.Text
      else
        Text := FDataLink.Field.DisplayText;
      FMemoLoaded := True;
    end;
  end
  else
  begin
    inherited SetPlainText(True);
    Text := FDataLink.Field.DisplayText;
    FMemoLoaded := True;
  end;
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME);
end;

procedure TJvDBRichEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not (FDataLink.Editing and FMemoLoaded);
end;

procedure TJvDBRichEdit.UpdateData(Sender: TObject);
begin
  if (FDataLink.Field <> nil) and FDataLink.Field.CanModify and not ReadOnly then
  begin
    if FDataLink.Field.IsBlob then
      FDataLink.Field.Assign(Lines)
    else
      FDataLink.Field.AsString := Text;
  end;
end;

procedure TJvDBRichEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if not Assigned(FDataLink.Field) or not FDataLink.Field.IsBlob then
      FDataLink.Reset;
  end;
end;

procedure TJvDBRichEdit.DoEnter;
begin
  SetFocused(True);
  inherited DoEnter;
  if SysLocale.FarEast and FDataLink.CanModify then
    inherited ReadOnly := False;
end;

procedure TJvDBRichEdit.DoExit;
begin
  try
    FDataLink.UpdateRecord;
  except
    if CanFocus then
      SetFocus;
    raise;
  end;
  SetFocused(False);
  inherited DoExit;
end;

procedure TJvDBRichEdit.SetAutoDisplay(Value: Boolean);
begin
  if Value <> FAutoDisplay then
  begin
    FAutoDisplay := Value;
    if FAutoDisplay then
      LoadMemo;
  end;
end;

procedure TJvDBRichEdit.SetPlainText(Value: Boolean);
begin
  if PlainText <> Value then
  begin
    inherited SetPlainText(Value);
    if FMemoLoaded then
      FDataLink.Reset;
  end;
end;

procedure TJvDBRichEdit.WMLButtonDblClk(var Msg: TWMLButtonDblClk);
begin
  if not FMemoLoaded then
    LoadMemo
  else
    inherited;
end;

procedure TJvDBRichEdit.DoClipboardCut;
begin
  EditCanModify;
  inherited DoClipboardCut;
end;

procedure TJvDBRichEdit.DoClipboardPaste;
begin
  EditCanModify;
  inherited DoClipboardPaste;
end;

procedure TJvDBRichEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Longint(FDataLink);
end;

procedure TJvDBRichEdit.UpdateMemo;
begin
  if FDataLink.Editing and FMemoLoaded then
    UpdateData(Self);
end;

function TJvDBRichEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TJvDBRichEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

procedure TJvDBRichEdit.EMSetCharFormat(var Msg: TMessage);
begin
  if FMemoLoaded then
    if not FUpdating then
      if EditCanModify then
        Change;
  inherited;
end;

procedure TJvDBRichEdit.EMSetParaFormat(var Msg: TMessage);
begin
  if FMemoLoaded then
    if not FUpdating then
      if EditCanModify then
        Change;
  inherited;
end;

end.

