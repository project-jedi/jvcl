{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBDatePickerEdit, released on 2002-10-04.

The Initial Developer of the Original Code is Oliver Giesen [giesen@lucatec.com]
Portions created by Oliver Giesen are Copyright (C) 2002 Lucatec GmbH.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2002-12-09

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ A data-aware variation of the DatePickerEdit component.

 Notable features:

 - The inherited NoDateText mechansim is enhanced and utilized to support proper
  handling of NULL values.

 - If EnforceRequired is set to True (default) you should not have to worry
  about setting ShowCheckbox. If the associated field has the Required flag set
  ShowCheckbox will automatically be False.
}

unit JvDBDatePickerEdit;

interface

uses
  Classes, Controls, Graphics, Db, DbCtrls, Messages,
  JvDatePickerEdit;

type
  TJvCustomDBDatePickerEdit = class(TJvCustomDatePickerEdit)
  private
    FDataLink: TFieldDataLink;
    FEnforceRequired: Boolean;
    procedure ValidateShowCheckbox; overload;
    function ValidateShowCheckbox(const AValue: Boolean): Boolean; overload;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetEnforceRequired(const AValue: Boolean);
    procedure WMCut(var AMessage: TMessage); message WM_CUT;
    procedure WMPaste(var AMessage: TMessage); message WM_PASTE;
    procedure WMUndo(var AMessage: TMessage); message WM_UNDO;
    procedure CMGetDataLink(var AMessage: TMessage); message CM_GETDATALINK;
  protected
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function IsLinked: Boolean;
    procedure Change; override;
    procedure DoKillFocus(const ANextControl: TWinControl); override;
    procedure DropDown; override;
    function EditCanModify: Boolean; override;
    procedure SetChecked(const AValue: Boolean); override;
    procedure SetShowCheckbox(const AValue: Boolean); override;
    procedure UpdateDisplay; override;
    function GetEnableValidation: Boolean; override;
    function ValidateDate(const ADate: TDateTime): Boolean; override;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property EnforceRequired: Boolean read FEnforceRequired write SetEnforceRequired default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsEmpty: Boolean; override;
  end;

  TJvDBDatePickerEdit = class(TJvCustomDBDatePickerEdit)
  public
    property Checked;
    property Date;
    property Dropped;
  published
    property AllowNoDate;
    property Anchors;
    property AutoSelect;
    property AutoSize default False;
    property BorderStyle;
    property Caret;
    property CharCase;
    property ClipboardCommands;
    property Color;
    property Constraints;
    property Cursor;
    property Ctl3D;
    property DataField;
    property DataSource;
    property DateFormat;
    property DisabledColor;
    property DisabledTextColor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EnableValidation;
    property EnforceRequired;
    property Font;
    property GroupIndex;
    property HintColor;
    property HotTrack;
//    property MaxYear default 2900;
//    property MinYear default 1900;
    property NoDateShortcut;
    property NoDateText;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property ShowCheckbox;
    property TabOrder;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCheckClick;
    property OnCtl3DChanged;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnKillFocus;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnParentColorChange;
    property OnSetFocus;
    property OnStartDrag;
  end;

implementation

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF}
  SysUtils;

constructor TJvCustomDBDatePickerEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FEnforceRequired := False;
  FDataLink := TFieldDataLink.Create;
  with FDataLink do
  begin
    Control := Self;
    OnDataChange := DataChange;
    OnUpdateData := UpdateData;
  end;
end;

destructor TJvCustomDBDatePickerEdit.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.OnUpdateData := nil;
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

procedure TJvCustomDBDatePickerEdit.Change;
begin
  if IsLinked then
    FDataLink.Modified;
  inherited Change;
end;

procedure TJvCustomDBDatePickerEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    Self.Date := FDataLink.Field.AsDateTime;
end;

function TJvCustomDBDatePickerEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TJvCustomDBDatePickerEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TJvCustomDBDatePickerEdit.IsEmpty: Boolean;
begin
  if IsLinked then
  begin
    if FDataLink.DataSet.State in [dsEdit, dsInsert] then
      Result := inherited IsEmpty
    else
      Result := FDataLink.Field.IsNull;
  end
  else
    Result := True;
end;

function TJvCustomDBDatePickerEdit.IsLinked: Boolean;
begin
  Result := Assigned(FDataLink) and Assigned(FDataLink.Field);
end;

procedure TJvCustomDBDatePickerEdit.DoKillFocus(const ANextControl: TWinControl);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited DoKillFocus(ANextControl);
end;

procedure TJvCustomDBDatePickerEdit.SetDataField(const AValue: string);
begin
  FDataLink.FieldName := AValue;
  ValidateShowCheckbox;
end;

procedure TJvCustomDBDatePickerEdit.SetDataSource(const AValue: TDataSource);
begin
  FDataLink.DataSource := AValue;
  ValidateShowCheckbox;
end;

procedure TJvCustomDBDatePickerEdit.SetEnforceRequired(const AValue: Boolean);
begin
  FEnforceRequired := AValue;
  ValidateShowCheckbox;
end;

procedure TJvCustomDBDatePickerEdit.SetShowCheckbox(const AValue: Boolean);
begin
  inherited SetShowCheckbox(ValidateShowCheckbox(AValue));
end;

procedure TJvCustomDBDatePickerEdit.UpdateData(Sender: TObject);
begin
  if IsLinked then
    if not Checked then
      FDataLink.Field.Value := NULL
    else
      FDataLink.Field.AsDateTime := Self.Date;
end;

procedure TJvCustomDBDatePickerEdit.UpdateDisplay;
begin
  if IsLinked then
    inherited UpdateDisplay
  else
  begin
    Checked := False;
    if not (csDesigning in ComponentState) then
      Text := EmptyStr;
  end;
end;

procedure TJvCustomDBDatePickerEdit.ValidateShowCheckbox;
begin
  inherited SetShowCheckbox(ValidateShowCheckbox(ShowCheckbox));
end;

function TJvCustomDBDatePickerEdit.ValidateShowCheckbox(
  const AValue: Boolean): Boolean;
begin
  Result := AValue;
  if EnforceRequired and IsLinked then
  begin
    if AValue and FDataLink.Field.Required then
      Result := False;
    AllowNoDate := not FDataLink.Field.Required;
  end;
end;

function TJvCustomDBDatePickerEdit.GetEnableValidation: Boolean;
begin
  Result := inherited GetEnableValidation;
  {if we enabled validation for an unlinked control, we'd have validation errors
   pop up just from tabbing over the control, therefore we temporary disable it}
  if InternalChanging or Leaving then
    Result := Result and IsLinked;
end;

function TJvCustomDBDatePickerEdit.ValidateDate(const ADate: TDateTime): Boolean;
begin
  result := (not IsLinked) or (FDataLink.DataSet.IsEmpty)
    or ((not Focused) and (FDataLink.DataSet.State = dsInsert) and FDataLink.Field.IsNull)
    or (inherited ValidateDate(ADate));
end;

procedure TJvCustomDBDatePickerEdit.CMGetDataLink(var AMessage: TMessage);
begin
  AMessage.Result := Integer(FDataLink);
end;

procedure TJvCustomDBDatePickerEdit.DropDown;
begin
  if EditCanModify then
    inherited DropDown;
end;

function TJvCustomDBDatePickerEdit.EditCanModify: Boolean;
begin
  result := (not IsLinked) or FDataLink.Edit;
end;

procedure TJvCustomDBDatePickerEdit.SetChecked(const AValue: Boolean);
begin
  if EditCanModify then
    inherited SetChecked(AValue)
  else
    UpdateDisplay;
end;

procedure TJvCustomDBDatePickerEdit.WMCut(var AMessage: TMessage);
begin
  if EditCanModify then
    inherited;
end;

procedure TJvCustomDBDatePickerEdit.WMPaste(var AMessage: TMessage);
begin
  if EditCanModify then
    inherited;
end;

procedure TJvCustomDBDatePickerEdit.WMUndo(var AMessage: TMessage);
begin
  if EditCanModify then
    inherited;
end;

end.

