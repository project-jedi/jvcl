{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBDatePickerEdit, released on 2002-10-04.

The Initial Developer of the Original Code is Oliver Giesen [giesen att lucatec dott com]
Portions created by Oliver Giesen are Copyright (C) 2002 Lucatec GmbH.
All Rights Reserved.

Contributor(s): ______________________________________.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  A data-aware variation of the DatePickerEdit component.

  Notable features:

  - The inherited NoDateText mechanism is enhanced and utilized to support proper
     handling of NULL values.

  - If EnforceRequired is set to True (default) you should not have to worry
    about setting ShowCheckBox. If the associated field has the Required flag set
    ShowCheckBox will automatically be False.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

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
    procedure ValidateShowCheckBox; overload;
    function ValidateShowCheckBox(const AValue: Boolean): Boolean; overload;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetEnforceRequired(const AValue: Boolean);
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
  protected
    procedure DoClipboardCut; override;
    procedure DoClipboardPaste; override;
    procedure DoUndo; override;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function IsLinked: Boolean;
    procedure Change; override;
    procedure DoKillFocusEvent(const ANextControl: TWinControl); override;
    procedure DropDown; override;
    function EditCanModify: Boolean; override;
    procedure SetChecked(const AValue: Boolean); override;
    procedure SetShowCheckBox(const AValue: Boolean); override;
    procedure UpdateDisplay; override;
    function GetEnableValidation: Boolean; override;
    function ValidateDate(const ADate: TDateTime): Boolean; override;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property EnforceRequired: Boolean read FEnforceRequired write SetEnforceRequired default False;
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
    property Action;
    property Align; 
    property AllowNoDate;
    property AlwaysReturnEditDate;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property ButtonFlat;
    property ButtonHint;
    property ButtonWidth;
    property CalendarAppearance;
    property Caret;
    property CharCase;
    property ClipboardCommands;
    property Color;
    property Constraints;
    //property Cursor; {already published}
    property DataField;
    property DataSource;
    property DateFormat;
    property DateSeparator;
    property DirectInput;
    property DisabledColor;
    property DisabledTextColor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EnableValidation;
    property EnforceRequired;
    property Font;
    property Glyph;
    property GroupIndex;
    property HideSelection;
    property HintColor;
    property HotTrack;
    // property MaxYear default 2900;
    // property MinYear default 1900;
    {$IFDEF VCL}
    {property BiDiMode;}
    property Ctl3D;
    {property ParentBiDiMode;}
    property ImeMode;
    property ImeName;
    property OEMConvert;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF VCL}
    property ImageIndex;
    property ImageKind;
    property Images;
    property NoDateShortcut;
    property NoDateText;
    property NumGlyphs;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property ShowCheckBox;
    property StoreDateFormat;
    property TabOrder;
    {property TabStop;} {(rb) Why disabled?}
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnCheckClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnabledChanged;
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
  {$ENDIF COMPILER6_UP}
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
  if IsLinked then
    Self.Date := FDataLink.Field.AsDateTime;
end;

function TJvCustomDBDatePickerEdit.GetDataField: string;
begin
  if FDataLink <> nil then
    Result := FDataLink.FieldName
  else
    Result := '';
end;

function TJvCustomDBDatePickerEdit.GetDataSource: TDataSource;
begin
  if FDataLink <> nil then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;

function TJvCustomDBDatePickerEdit.IsEmpty: Boolean;
begin
  if IsLinked then
  begin
    if FDataLink.Editing then
      Result := inherited IsEmpty
    else
      try
        Result := FDataLink.DataSet.IsEmpty or FDataLink.Field.IsNull;
      except 
        Result := True;
      end;
  end
  else
    Result := True;
end;

function TJvCustomDBDatePickerEdit.IsLinked: Boolean;
begin
  Result := Assigned(FDataLink) and Assigned(FDataLink.Field);
end;

procedure TJvCustomDBDatePickerEdit.DoKillFocusEvent(const ANextControl: TWinControl);
begin
  if IsLinked and FDataLink.Editing then
    try
      FDataLink.UpdateRecord;
    except
      SetFocus;
      raise;
    end;
  inherited DoKillFocusEvent(ANextControl);
end;

procedure TJvCustomDBDatePickerEdit.SetDataField(const AValue: string);
begin
  FDataLink.FieldName := AValue;
  ValidateShowCheckBox;
end;

procedure TJvCustomDBDatePickerEdit.SetDataSource(const AValue: TDataSource);
begin
  FDataLink.DataSource := AValue;
  ValidateShowCheckBox;
end;

procedure TJvCustomDBDatePickerEdit.SetEnforceRequired(const AValue: Boolean);
begin
  FEnforceRequired := AValue;
  ValidateShowCheckBox;
end;

procedure TJvCustomDBDatePickerEdit.SetShowCheckBox(const AValue: Boolean);
begin
  inherited SetShowCheckBox(ValidateShowCheckBox(AValue));
end;

procedure TJvCustomDBDatePickerEdit.UpdateData(Sender: TObject);
begin
  if IsLinked and FDataLink.Editing then
    if not Checked then
      FDataLink.Field.Value := Null
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
      Text := '';
  end;
end;

procedure TJvCustomDBDatePickerEdit.ValidateShowCheckBox;
begin
  inherited SetShowCheckBox(ValidateShowCheckBox(ShowCheckBox));
end;

function TJvCustomDBDatePickerEdit.ValidateShowCheckBox(const AValue: Boolean): Boolean;
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
  {if we enabled "as-you-type" validation for an unlinked control, we'd have
   validation errors pop up just from tabbing over the control, therefore we
   temporary disable it}
  if InternalChanging or Leaving then
    Result := Result and IsLinked and FDataLink.Editing;
end;

function TJvCustomDBDatePickerEdit.ValidateDate(const ADate: TDateTime): Boolean;
begin
  Result := (not IsLinked) or (FDataLink.DataSet.IsEmpty) or
    (not FDataLink.Editing) or
    ((not Focused) and (FDataLink.DataSet.State = dsInsert) and FDataLink.Field.IsNull) or
    (inherited ValidateDate(ADate));
end;

procedure TJvCustomDBDatePickerEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Integer(FDataLink);
end;

procedure TJvCustomDBDatePickerEdit.DropDown;
begin
  if EditCanModify then
    inherited DropDown;
end;

function TJvCustomDBDatePickerEdit.EditCanModify: Boolean;
begin
  Result := (not IsLinked) or FDataLink.Edit;
end;

procedure TJvCustomDBDatePickerEdit.SetChecked(const AValue: Boolean);
begin
  if AValue <> Checked then
  begin
    if EditCanModify then
      inherited SetChecked(AValue)
    else
      UpdateDisplay;
  end;
end;

procedure TJvCustomDBDatePickerEdit.DoClipboardCut;
begin
  if EditCanModify then
    inherited DoClipboardCut;
end;

procedure TJvCustomDBDatePickerEdit.DoClipboardPaste;
begin
  if EditCanModify then
    inherited DoClipboardPaste;
end;

procedure TJvCustomDBDatePickerEdit.DoUndo;
begin
  if EditCanModify then
    inherited DoUndo;
end;

end.

