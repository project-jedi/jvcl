{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBComb.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Polaris Software

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBCombobox;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages,
  Variants, Classes, Graphics, Controls, StdCtrls, DB, DBCtrls,
  JvExStdCtrls, JvDBUtils, JvCombobox;

type
  TJvCustomDBComboBox = class;
  TJvDBComboBox = class;

  TJvComboBoxFilterEvent = procedure(Sender: TObject; DataSet: TDataSet; var Accept: Boolean) of object;

  TJvDBComboBoxListDataLink = class(TDataLink)
  private
    FOnReload: TNotifyEvent;
  protected
    procedure DataEvent(Event: TDataEvent; Info: {$IFDEF RTL230_UP}NativeInt{$ELSE}Integer{$ENDIF}); override;
    procedure ActiveChanged; override;
  public
    property OnReload: TNotifyEvent read FOnReload write FOnReload;
  end;

  TJvDBComboBoxListSettings = class(TPersistent)
  private
    FListDataLink: TJvDBComboBoxListDataLink;
    FFilter: string;
    FKeyField: string;
    FDisplayField: string;
    FOnFilter: TJvComboBoxFilterEvent;
    FShowOutfilteredValue: Boolean;
    FOutfilteredValueFont: TFont;
    FComboBox: TJvCustomDBComboBox;
    FOutfilteredItems: TStrings;
    FOutfilteredValues: TStrings;
    FUseDisplayText: Boolean;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetFilter(Value: string);
    function GetDataSource: TDataSource;
    procedure SetDisplayField(const Value: string);
    procedure SetKeyField(const Value: string);
    procedure SetShowOutfilteredValue(const Value: Boolean);
    procedure SetOutfilteredValueFont(const Value: TFont);
    procedure SetUseDisplayText(const Value: Boolean);
  protected
    procedure ListDataChange(Sender: TObject);
    procedure FontChange(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation);

    property ComboBox: TJvCustomDBComboBox read FComboBox;
  public
    constructor Create(AComboBox: TJvCustomDBComboBox);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function IsValid: Boolean;
  published
    { ShowOutfilteredValue: Shows the value/item if the field value is not in the
      filtered dataset but in the unfiltered dataset. }
    property ShowOutfilteredValue: Boolean read FShowOutfilteredValue write SetShowOutfilteredValue default False;
    { OutfilteredValueFont: The font that is used to paint the out-filtered value/item. }
    property OutfilteredValueFont: TFont read FOutfilteredValueFont write SetOutfilteredValueFont;

    { Filter: Is used to filter the dataset. It is compatible to the TClientDataSet.Filter }
    property Filter: string read FFilter write SetFilter;
    { KeyField: The field that is used for the ComboBox.Values list. }
    property KeyField: string read FKeyField write SetKeyField;
    { DisplayField: The field that is used for the ComboBox.Items list. }
    property DisplayField: string read FDisplayField write SetDisplayField;
    { DataSource: The records of the data source are filtered and added to the
      ComboBox.Values/Items list. }
    property DataSource: TDataSource read GetDataSource write SetDataSource;

    { UseDisplayText: Use DisplayField.DisplayText instead of DisplayField.AsString. }
    property UseDisplayText: Boolean read FUseDisplayText write SetUseDisplayText default False;

    { OnFilter is triggered for every record before the Filter property is applied. }
    property OnFilter: TJvComboBoxFilterEvent read FOnFilter write FOnFilter;
  end;

  TJvCustomDBComboBox = class(TJvCustomComboBox, IJvDataControl)
  private
    FDataLink: TFieldDataLink;
    FPaintControl: TPaintControl;
    FBeepOnError: Boolean;
    FResetValue: Boolean;
    FUpdateFieldImmediatelly: Boolean;
    FListSettings: TJvDBComboBoxListSettings;
    FValues: TStringList;
    FEnableValues: Boolean;
    FPreserveItemSelectionOnInsert: Boolean;
    procedure SetEnableValues(Value: Boolean);
    function GetValues: TStrings;
    procedure SetValues(Value: TStrings);
    procedure ValuesChanged(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetEditReadOnly;
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    function GetComboText: string;
    procedure SetComboText(const Value: string);
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure SetListSettings(const Value: TJvDBComboBoxListSettings);
  protected
    function GetDataLink: TDataLink;

    procedure DoExit; override;
    procedure Change; override;
    procedure Click; override;
    procedure Reset;

    // This may cause trouble with BCB because it uses a HWND parameter
    // but as it is defined in the VCL itself, we can't do much.
    procedure ComboWndProc(var Msg: TMessage; ComboWnd: HWND; ComboProc: Pointer); override;

    procedure CreateWnd; override;
    function GetPaintText: string; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetStyle(Value: TComboBoxStyle); override;
    function FilterAccepted: Boolean; virtual;

    procedure SetItems(const Value: TStrings); override;
    procedure WndProc(var Msg: TMessage); override;
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default False;
    property ComboText: string read GetComboText write SetComboText;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ListSettings: TJvDBComboBoxListSettings read FListSettings write SetListSettings;
    property Values: TStrings read GetValues write SetValues;
    property EnableValues: Boolean read FEnableValues write SetEnableValues default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateDropDownItems; virtual;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;

    property Field: TField read GetField;
    property Items write SetItems;
    property Text;
    property UpdateFieldImmediatelly: Boolean read FUpdateFieldImmediatelly write FUpdateFieldImmediatelly default False;
    property PreserveItemSelectionOnInsert: Boolean read FPreserveItemSelectionOnInsert write FPreserveItemSelectionOnInsert default False;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDBComboBox = class(TJvCustomDBComboBox)
  private
    { The "AutoSize" property was published what it never should have been. TComboBox doesn't
      support it and TJvCustomComboBox/TJvDBCustomComboBox do not support it either. }
    procedure ReadIgnoredBoolean(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  published
    property Align;
    property Style default csDropDownList; { must be published before Items }
    property BeepOnError;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Color;
    property DataField;
    property DataSource;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property DropDownWidth;
    property Enabled;
    property EnableValues;
    property Font;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property Items;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property PreserveItemSelectionOnInsert;
    property ReadOnly;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property UpdateFieldImmediatelly;
    property Values;
    property Visible;
    property ListSettings; { should be published after Items and Values }
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;
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
  VDBConsts,
  {$IFNDEF COMPILER12_UP}
  JvJCLUtils,
  {$ENDIF ~COMPILER12_UP}
  SysUtils,
  JvDBFilterExpr,
  JvConsts;

type
  TDataSetAccess = class(TDataSet);

//=== { TJvCustomDBComboBox } ================================================

constructor TJvCustomDBComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnEditingChange := EditingChange;
  FPaintControl := TPaintControl.Create(Self, 'COMBOBOX');
  FBeepOnError := False;

  FListSettings := TJvDBComboBoxListSettings.Create(Self);
  FValues := TStringList.Create;
  FValues.OnChange := ValuesChanged;
  FEnableValues := True;
  Style := csDropDownList;
end;

destructor TJvCustomDBComboBox.Destroy;
begin
  FPaintControl.Free;
  FDataLink.OnDataChange := nil;
  FDataLink.OnUpdateData := nil;
  FDataLink.Free;
  FDataLink := nil;
  FreeAndNil(FListSettings);
  FValues.OnChange := nil;
  FValues.Free;
  inherited Destroy;
end;

procedure TJvCustomDBComboBox.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);
  UpdateDropDownItems;
end;

procedure TJvCustomDBComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;

  if FListSettings <> nil then
    FListSettings.Notification(AComponent, Operation);
end;

procedure TJvCustomDBComboBox.CreateWnd;
begin
  inherited CreateWnd;
  SetEditReadOnly;
  DataChange(Self);
end;

procedure TJvCustomDBComboBox.DataChange(Sender: TObject);
begin
  if not HandleAllocated or (DroppedDown and not FResetValue) then
    Exit;
  if FDataLink.Field <> nil then
    SetComboText(FDataLink.Field.AsString)
  else
  if csDesigning in ComponentState then
    ComboText := Name
  else
  if FDataLink <> nil then
  begin
    FDataLink.UpdateRecord;
    if (FDataLink.DataSource <> nil) and not FDataLink.DataSource.Enabled then
      ComboText := '';
  end
  else
    ComboText := '';
end;

procedure TJvCustomDBComboBox.UpdateData(Sender: TObject);
begin
  FDataLink.Field.AsString := ComboText;
end;

procedure TJvCustomDBComboBox.SetComboText(const Value: string);
var
  I: Integer;
  Redraw: Boolean;
begin
  if Value <> ComboText then
  begin
    if Style <> csDropDown then
    begin
      Redraw := (Style <> csSimple) and HandleAllocated;
      if Redraw then
        SendMessage(Handle, WM_SETREDRAW, 0, 0);
      try
        if Value = '' then
          I := -1
        else
        begin
          I := Items.IndexOf(Value);
          if (I = -1) and FEnableValues then
            I := Values.IndexOf(Value);
        end;
        if I >= Items.Count then
          I := -1;
        ItemIndex := I;
      finally
        if Redraw then
        begin
          SendMessage(Handle, WM_SETREDRAW, 1, 0);
          Invalidate;
        end;
      end;
      if I >= 0 then
        Exit;
    end;
    if Style in [csDropDown, csSimple] then
      Text := Value;
  end;
end;

function TJvCustomDBComboBox.GetComboText: string;
var
  I: Integer;
begin
  if (Style in [csDropDown, csSimple]) and not FEnableValues then
    Result := Text
  else
  begin
    I := ItemIndex;
    if (I < 0) or (FEnableValues and (FValues.Count < I + 1)) then
      Result := ''
    else
    if FEnableValues then
      Result := FValues[I]
    else
      Result := Items[I];
  end;
end;

procedure TJvCustomDBComboBox.Change;
begin
  FDataLink.Edit;
  if UpdateFieldImmediatelly then
    FDataLink.UpdateRecord;
  inherited Change;
  FDataLink.Modified;
end;

procedure TJvCustomDBComboBox.Click;
begin
  FDataLink.Edit;
  if UpdateFieldImmediatelly then
    FDataLink.UpdateRecord;
  inherited Click;
  FDataLink.Modified;
end;

function TJvCustomDBComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvCustomDBComboBox.SetDataSource(Value: TDataSource);
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

function TJvCustomDBComboBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvCustomDBComboBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TJvCustomDBComboBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJvCustomDBComboBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TJvCustomDBComboBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJvCustomDBComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key in [VK_BACK, VK_DELETE, VK_UP, VK_DOWN, 32..255] then
    if not FDataLink.Edit and (Key in [VK_UP, VK_DOWN]) then
      Key := 0;
end;

procedure TJvCustomDBComboBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key >= #32) and (FDataLink.Field <> nil) and not FDataLink.Field.IsValidChar(Key) then
  begin
    if BeepOnError then
      SysUtils.Beep;
    Key := #0;
  end;
  case Key of
    CtrlH, CtrlV, CtrlX, #32..High(Char):
      FDataLink.Edit;
    Esc:
      begin
        FDataLink.Reset;
        if UpdateFieldImmediatelly and (FDataLink.Field <> nil) and FDataLink.Editing then
          FDataLink.Field.Value := FDataLink.Field.OldValue;
        SelectAll;
      end;
  end;
end;

procedure TJvCustomDBComboBox.EditingChange(Sender: TObject);
begin
  SetEditReadOnly;
end;

procedure TJvCustomDBComboBox.SetEditReadOnly;
begin
  if (Style in [csDropDown, csSimple]) and HandleAllocated then
    SendMessage(EditHandle, EM_SETREADONLY, Ord(not FDataLink.Editing), 0);
end;

procedure TJvCustomDBComboBox.Reset;
begin
  FResetValue := True;
  try
    DataChange(Self); {Restore text}
  finally
    FResetValue := False;
  end;
end;

procedure TJvCustomDBComboBox.WndProc(var Msg: TMessage);
var
  OldItemIndex: Integer;
begin
  if not (csDesigning in ComponentState) then
    case Msg.Msg of
      WM_COMMAND:
        if TWMCommand(Msg).NotifyCode = CBN_SELCHANGE then
        begin
          try
            if FPreserveItemSelectionOnInsert and FDataLink.Active and FDataLink.CanModify and (FDataLink.DataSet.State = dsBrowse) then
              OldItemIndex := ItemIndex
            else
              OldItemIndex := -1;

            if not FDataLink.Edit then
            begin
              if Style <> csSimple then
                PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
              Exit;
            end;

            // Restore ItemIndex if FDataLink.Edit has triggered OnNewRecord that changed the associated field value.
            // Otherwise the user's selection is reverted to the value in OnNewRecord when the dropdown list is closed
            if FPreserveItemSelectionOnInsert and (OldItemIndex <> -1) and (OldItemIndex <> ItemIndex) and (FDataLink.DataSet.State = dsInsert) then
              ItemIndex := OldItemIndex;
          except
            Reset;
            raise;
          end;
        end;
      CB_SHOWDROPDOWN:
        if Msg.WParam <> 0 then
        begin
          try
            FDataLink.Edit;
          except
            Reset;
            raise;
          end;
        end
        else
        if not FDataLink.Editing then
          Reset;
      WM_CREATE, WM_WINDOWPOSCHANGED, CM_FONTCHANGED:
        FPaintControl.DestroyHandle;
    end;
  inherited WndProc(Msg);
end;

procedure TJvCustomDBComboBox.ComboWndProc(var Msg: TMessage; ComboWnd: HWND; ComboProc: Pointer);
begin
  if not (csDesigning in ComponentState) then
    case Msg.Msg of
      WM_LBUTTONDOWN:
        if (Style = csSimple) and (ComboWnd <> EditHandle) then
          if not FDataLink.Edit then
            Exit;
    end;
  inherited ComboWndProc(Msg, ComboWnd, ComboProc);
end;

procedure TJvCustomDBComboBox.DoExit;
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then
      SetFocus;
    raise;
  end;
  inherited DoExit;
  if ListSettings.IsValid and ListSettings.ShowOutfilteredValue and (ItemIndex = -1) then
    Invalidate;
end;

procedure TJvCustomDBComboBox.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := LRESULT(FDataLink);
end;

function TJvCustomDBComboBox.GetDataLink: TDataLink;
begin
  Result := FDataLink;
end;

procedure TJvCustomDBComboBox.WMPaint(var Msg: TWMPaint);

  procedure DefaultPaint;
  var
    S: string;
    R: TRect;
    P: TPoint;
    Child: HWND;
  begin
    if csPaintCopy in ControlState then
    begin
      S := GetPaintText;
      if Style = csDropDown then
      begin
        SendMessage(FPaintControl.Handle, WM_SETTEXT, 0, LPARAM(PChar(S)));
        SendMessage(FPaintControl.Handle, WM_PAINT, Msg.DC, 0);
        Child := GetWindow(FPaintControl.Handle, GW_CHILD);
        if Child <> 0 then
        begin
          Windows.GetClientRect(Child, R);
          Windows.MapWindowPoints(Child, FPaintControl.Handle, R.TopLeft, 2);
          GetWindowOrgEx(Msg.DC, P);
          SetWindowOrgEx(Msg.DC, P.X - R.Left, P.Y - R.Top, nil);
          IntersectClipRect(Msg.DC, 0, 0, R.Right - R.Left, R.Bottom - R.Top);
          SendMessage(Child, WM_PAINT, Msg.DC, 0);
        end;
      end
      else
      begin
        SendMessage(FPaintControl.Handle, CB_RESETCONTENT, 0, 0);
        if Items.IndexOf(S) <> -1 then
        begin
          SendMessage(FPaintControl.Handle, CB_ADDSTRING, 0, LPARAM(PChar(S)));
          SendMessage(FPaintControl.Handle, CB_SETCURSEL, 0, 0);
        end;
        SendMessage(FPaintControl.Handle, WM_PAINT, Msg.DC, 0);
      end;
    end
    else
      inherited;
  end;

var
  S: string;
  R: TRect;
  PaintStruct: TPaintStruct;
  DC: HDC;
  OldFont: HFONT;
  Index: Integer;
begin
  { If the field value is not part of the DataSource }
  if (Style in [csDropDownList, csOwnerDrawFixed, csOwnerDrawVariable]) and
     ListSettings.ShowOutfilteredValue and (ItemIndex = -1) and
     FDataLink.Active and (FDataLink.Field <> nil) and not FDataLink.Field.IsNull and
     ListSettings.IsValid then
  begin
    if ListSettings.DisplayField <> '' then
    begin
      Index := ListSettings.FOutfilteredValues.IndexOf(FDataLink.Field.AsString);
      if (Index <> -1) and (Index < Items.Count) then
        S := ListSettings.FOutfilteredItems[Index];
    end
    else
      S := FDataLink.Field.Text;

    if Trim(S) = '' then
    begin
      DefaultPaint;
      Exit;
    end;

    DC := Msg.DC;
    if DC = 0 then
      DC := BeginPaint(Handle, PaintStruct);
    try
      Msg.DC := DC;
      DefaultPaint;

      R := ClientRect;
      InflateRect(R, -1, -1);
      Inc(R.Left, 3);
      SetTextColor(DC, ColorToRGB(ListSettings.OutfilteredValueFont.Color));
      SetBkMode(DC, TRANSPARENT);
      OldFont := SelectObject(DC, ListSettings.OutfilteredValueFont.Handle);
      if Style = csDropDownList then
        DrawText(DC, PChar(S), Length(S), R, DT_VCENTER or DT_END_ELLIPSIS or DT_SINGLELINE or DT_NOPREFIX)
      else
      begin
        Inc(R.Left);
        R.Top := 3;
        DrawText(DC, PChar(S), Length(S), R, DT_END_ELLIPSIS or DT_SINGLELINE or DT_NOPREFIX)
      end;
      SelectObject(DC, OldFont);
    finally
      if PaintStruct.hdc <> 0 then
        EndPaint(Handle, PaintStruct);
    end;
  end
  else
    DefaultPaint;
end;

function TJvCustomDBComboBox.GetPaintText: string;
var
  I: Integer;
begin
  Result := '';
  if FDataLink.Field <> nil then
  begin
    if FEnableValues then
    begin
      I := Values.IndexOf(FDataLink.Field.AsString);
      if I >= 0 then
        Result := Items.Strings[I];
    end
    else
      Result := FDataLink.Field.Text;
  end;
end;

procedure TJvCustomDBComboBox.SetItems(const Value: TStrings);
begin
  inherited SetItems(Value);
  DataChange(Self);
end;

procedure TJvCustomDBComboBox.SetStyle(Value: TComboBoxStyle);
begin
  if (Value in [csSimple, csDropDown]) and FEnableValues then
    FEnableValues := False;

  if (Value = csSimple) and Assigned(FDataLink) and FDataLink.DataSourceFixed then
    _DBError(SNotReplicatable);
  inherited SetStyle(Value);
end;

function TJvCustomDBComboBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TJvCustomDBComboBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TJvCustomDBComboBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

procedure TJvCustomDBComboBox.ValuesChanged(Sender: TObject);
begin
  if FEnableValues then
    DataChange(Self);
end;

procedure TJvCustomDBComboBox.SetEnableValues(Value: Boolean);
begin
  if FEnableValues <> Value then
  begin
    if Value and (Style in [csDropDown, csSimple]) then
      Style := csDropDownList;
    FEnableValues := Value;
    DataChange(Self);
  end;
end;

procedure TJvCustomDBComboBox.SetListSettings(const Value: TJvDBComboBoxListSettings);
begin
  if Value <> FListSettings then
    FListSettings.Assign(Value);
end;

function TJvCustomDBComboBox.GetValues: TStrings;
begin
  Result := FValues;
end;

procedure TJvCustomDBComboBox.SetValues(Value: TStrings);
begin
  FValues.Assign(Value);
end;

function TJvCustomDBComboBox.FilterAccepted: Boolean;
begin
  Result := True;
  with ListSettings do
    if Assigned(FOnFilter) and (DataSource <> nil) and (DataSource.DataSet <> nil) then
      FOnFilter(Self, DataSource.DataSet, Result);
end;

procedure TJvCustomDBComboBox.UpdateDropDownItems;
var
  Bookmark: TBookmark;
  FilterExpr: TJvDBFilterExpression;
  LKeyField, LDisplayField: TField;
  DataSet: TDataSet;
  LastText: string;
begin
  if ([csDesigning, csLoading, csDestroying] * ComponentState = []) and
     (ListSettings.DataSource <> nil) and (ListSettings.DataSource.DataSet <> nil) and
     (ListSettings.DataSource.State = dsBrowse) then
  begin
    ListSettings.FOutfilteredItems.Clear;
    ListSettings.FOutfilteredValues.Clear;
    { Component is in the ListDataSet mode }
    Items.BeginUpdate;
    Values.BeginUpdate;
    try
      LastText := GetComboText;
      Items.Clear;
      Values.Clear;
      if ListSettings.IsValid and ListSettings.DataSource.DataSet.Active and (ListSettings.KeyField <> '') then
      begin
        DataSet := ListSettings.DataSource.DataSet;
        LKeyField := DataSet.FieldByName(ListSettings.KeyField);
        if ListSettings.DisplayField = '' then
          LDisplayField := LKeyField
        else
          LDisplayField := DataSet.FieldByName(ListSettings.DisplayField);

        DataSet.DisableControls;
        try
          Bookmark := DataSet.GetBookmark;
          try
            FilterExpr := nil;
            if ListSettings.Filter <> '' then
              FilterExpr := TJvDBFilterExpression.Create(DataSet, ListSettings.Filter, []);
            try
              DataSet.First;
              while not DataSet.Eof do
              begin
                if FilterAccepted and ((FilterExpr = nil) or FilterExpr.Evaluate) then
                begin
                  if ListSettings.UseDisplayText then
                    Items.Add(LDisplayField.DisplayText)
                  else
                    Items.Add(LDisplayField.AsString);
                  Values.Add(LKeyField.AsString);
                end
                else
                begin
                  if ListSettings.UseDisplayText then
                    ListSettings.FOutfilteredItems.Add(LDisplayField.DisplayText)
                  else
                    ListSettings.FOutfilteredItems.Add(LDisplayField.AsString);
                  ListSettings.FOutfilteredValues.Add(LKeyField.AsString);
                end;
                DataSet.Next;
              end;
            finally
              FilterExpr.Free;
            end;
          finally
            if Bookmark <> nil then
            begin
              DataSet.GotoBookmark(Bookmark);
              DataSet.FreeBookmark(Bookmark);
            end;
          end;
        finally
          //DataSet.EnableControls;
          TDataSetAccess(DataSet).RestoreState(DataSet.State); // do not trigger a refresh
        end;
      end;
    finally
      Items.EndUpdate;
      Values.EndUpdate;
    end;
    if ItemIndex = -1 then
      SetComboText(LastText);
  end;
end;

{ TJvDBComboBoxListDataLink }

procedure TJvDBComboBoxListDataLink.ActiveChanged;
begin
  inherited ActiveChanged;
  if Assigned(FOnReload) then
    FOnReload(Self);
end;

procedure TJvDBComboBoxListDataLink.DataEvent(Event: TDataEvent; Info: {$IFDEF RTL230_UP}NativeInt{$ELSE}Integer{$ENDIF});
begin
  inherited DataEvent(Event, Info);
  if Assigned(FOnReload) then
  begin
    case Event of
      deFieldListChange,
      deDataSetChange:
        FOnReload(Self);
    end;
  end;
end;

{ TJvDBComboBoxListSettings }

constructor TJvDBComboBoxListSettings.Create(AComboBox: TJvCustomDBComboBox);
begin
  inherited Create;
  FComboBox := AComboBox;
  FListDataLink := TJvDBComboBoxListDataLink.Create;
  FListDataLink.OnReload := ListDataChange;
  FShowOutfilteredValue := False;
  FOutfilteredValueFont := TFont.Create;
  FOutfilteredValueFont.Color := clRed;
  FOutfilteredValueFont.OnChange := FontChange;
  FOutfilteredItems := TStringList.Create;
  FOutfilteredValues := TStringList.Create;
end;

destructor TJvDBComboBoxListSettings.Destroy;
begin
  SetDataSource(nil);
  FOutfilteredValueFont.Free;
  FListDataLink.OnReload := nil;
  FListDataLink.Free;
  FListDataLink := nil;
  FOutfilteredItems.Free;
  FOutfilteredValues.Free;
  inherited Destroy;
end;

procedure TJvDBComboBoxListSettings.FontChange(Sender: TObject);
begin
  ComboBox.Invalidate;
end;

procedure TJvDBComboBoxListSettings.Assign(Source: TPersistent);
var
  Src: TJvDBComboBoxListSettings;
begin
  if Source is TJvDBComboBoxListSettings then
  begin
    Src := TJvDBComboBoxListSettings(Source);
    FShowOutfilteredValue := Src.FShowOutfilteredValue;
    FOutfilteredValueFont.Assign(Src.FOutfilteredValueFont);
    FFilter := Src.FFilter;
    FKeyField := Src.FKeyField;
    FDisplayField := Src.FDisplayField;
    FUseDisplayText := Src.FUseDisplayText;
    SetDataSource(Src.DataSource);
    FOnFilter := Src.FOnFilter;

    ComboBox.UpdateDropDownItems;
    ComboBox.Invalidate;
  end
  else
    inherited Assign(Source);
end;

procedure TJvDBComboBoxListSettings.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
    if AComponent = DataSource then
      SetDataSource(nil);
end;

procedure TJvDBComboBoxListSettings.SetDataSource(const Value: TDataSource);
begin
  if Value <> DataSource then
  begin
    if DataSource <> nil then
    begin
      DataSource.RemoveFreeNotification(ComboBox);
      FListDataLink.DataSource := nil;
    end;
    FListDataLink.DataSource := Value;
    if DataSource <> nil then
      DataSource.FreeNotification(ComboBox);
  end;
end;

procedure TJvDBComboBoxListSettings.SetDisplayField(const Value: string);
begin
  if Value <> FDisplayField then
  begin
    FDisplayField := Value;
    ComboBox.UpdateDropDownItems;
  end;
end;

procedure TJvDBComboBoxListSettings.SetFilter(Value: string);
begin
  Value := Trim(Value);
  if Value <> FFilter then
  begin
    FFilter := Value;
    ComboBox.UpdateDropDownItems;
    if ComboBox.UpdateFieldImmediatelly then
      ComboBox.DataChange(Self);
  end;
end;

procedure TJvDBComboBoxListSettings.SetKeyField(const Value: string);
begin
  if Value <> FKeyField then
  begin
    FKeyField := Value;
    ComboBox.UpdateDropDownItems;
  end;
end;

procedure TJvDBComboBoxListSettings.SetOutfilteredValueFont(const Value: TFont);
begin
  if Value <> FOutfilteredValueFont then
  begin
    FOutfilteredValueFont.Assign(Value);
    ComboBox.Invalidate;
  end;
end;

procedure TJvDBComboBoxListSettings.SetShowOutfilteredValue(const Value: Boolean);
begin
  if Value <> FShowOutfilteredValue then
  begin
    FShowOutfilteredValue := Value;
    ComboBox.Invalidate;
  end;
end;

procedure TJvDBComboBoxListSettings.SetUseDisplayText(const Value: Boolean);
begin
  if Value <> FUseDisplayText then
  begin
    FUseDisplayText := Value;
    ComboBox.UpdateDropDownItems;
  end;
end;

function TJvDBComboBoxListSettings.GetDataSource: TDataSource;
begin
  if FListDataLink <> nil then
    Result := FListDataLink.DataSource
  else
    Result := nil;
end;

function TJvDBComboBoxListSettings.IsValid: Boolean;
begin
  Result := (DataSource <> nil) and (DataSource.DataSet <> nil) and DataSource.DataSet.Active and
            (KeyField <> '');
end;

procedure TJvDBComboBoxListSettings.ListDataChange(Sender: TObject);
begin
  if FListDataLink.Active and (DataSource.State = dsBrowse) then
  begin
    ComboBox.UpdateDropDownItems;
    if ComboBox.UpdateFieldImmediatelly then
      ComboBox.DataChange(Self);
  end;
end;

{ TJvDBComboBox }

procedure TJvDBComboBox.ReadIgnoredBoolean(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

procedure TJvDBComboBox.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('AutoSize', ReadIgnoredBoolean, nil, False);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
