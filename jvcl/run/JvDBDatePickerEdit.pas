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
located at http://jvcl.delphi-jedi.org

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

unit JvDBDatePickerEdit;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Controls, DB, DBCtrls,
  JvDatePickerEdit;

type
  TJvCustomDBDatePickerEdit = class(TJvCustomDatePickerEdit)
  private
    FDataLink: TFieldDataLink;
    FEnforceRequired: Boolean;
    FAllowPopupBrowsing: Boolean;
    FLockEditing: Integer;
    procedure ValidateShowCheckBox; overload;
    function ValidateShowCheckBox(const AValue: Boolean): Boolean; overload;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetEnforceRequired(const AValue: Boolean);
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
    function GetInternalDate: TDateTime;
    procedure SetInternalDate(const Value: TDateTime);
    function GetField: TField;
  protected
    procedure WMCut(var Msg: TMessage); message WM_CUT;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure WMUndo(var Msg: TMessage); message WM_UNDO;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function IsLinked: Boolean;
    procedure KeyPress(var Key: Char); override;
    procedure Change; override;
    procedure DoExit; override;
    procedure PopupDropDown(DisableEdit: Boolean); override;
    function EditCanModify: Boolean; override;
    procedure SetChecked(const AValue: Boolean); override;
    procedure SetShowCheckbox(const AValue: Boolean); override;
    procedure UpdateDisplay; override;
    function GetEnableValidation: Boolean; override;
    function ValidateDate(const ADate: TDateTime): Boolean; override;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property EnforceRequired: Boolean read FEnforceRequired write SetEnforceRequired default False;
    property AllowPopupBrowsing: Boolean read FAllowPopupBrowsing write FAllowPopupBrowsing default True;
    procedure Loaded; override;

    property InternalDate: TDateTime read GetInternalDate write SetInternalDate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsEmpty: Boolean; override;
    property Field: TField read GetField;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
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
    property AllowPopupBrowsing;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    {property BiDiMode;}
    property Flat;
    {property ParentBiDiMode;}
    property ImeMode;
    property ImeName;
    property OEMConvert;
    property ParentCtl3D;
    property OnEndDock;
    property OnStartDock;
    property ImageIndex;
    property ImageKind;
    property Images;
    property NoDateShortcut;
    property NoDateText;
    property NoDateValue;
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
    property TabStop;
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

    property OnGetValidDateString;
    property OnPopupShown;
    property OnPopupHidden;
    property OnPopupChange;
    property OnPopupValueAccepted;
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
  Variants, SysUtils;

function IsNullOrEmptyStringField(Field: TField): Boolean;
begin
  Result := Field.IsNull or ((Field is TStringField) and (Trim(Field.AsString) = ''));
end;

//=== { TJvCustomDBDatePickerEdit } ==========================================

procedure TJvCustomDBDatePickerEdit.Change;
begin
  if IsLinked then
    FDataLink.Modified;
  inherited Change;
end;

procedure TJvCustomDBDatePickerEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := LRESULT(FDataLink);
end;

constructor TJvCustomDBDatePickerEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FEnforceRequired := False;
  FAllowPopupBrowsing := True;
  FDataLink := TFieldDataLink.Create;
  with FDataLink do
  begin
    Control := Self;
    OnDataChange := DataChange;
    OnUpdateData := UpdateData;
  end;
end;

procedure TJvCustomDBDatePickerEdit.Loaded;
begin
  inherited Loaded;
  if AllowNoDate and not IsLinked then
    InternalDate := NoDateValue;
end;

procedure TJvCustomDBDatePickerEdit.DataChange(Sender: TObject);
begin
  if IsLinked and FDataLink.Active then
  begin
    if AllowNoDate and IsNullOrEmptyStringField(FDataLink.Field) then
      InternalDate := NoDateValue
    else
      InternalDate := FDataLink.Field.AsDateTime;
  end;
end;

destructor TJvCustomDBDatePickerEdit.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.OnUpdateData := nil;
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

procedure TJvCustomDBDatePickerEdit.WMCut(var Msg: TMessage);
begin
  if EditCanModify then
    inherited;
end;

procedure TJvCustomDBDatePickerEdit.WMPaste(var Msg: TMessage);
begin
  if EditCanModify then
    inherited;
end;

procedure TJvCustomDBDatePickerEdit.DoExit;
begin
  inherited DoExit;
  if IsLinked and FDataLink.Editing then
    try
      FDataLink.UpdateRecord;
    except
      SetFocus;
      raise;
    end;
end;

procedure TJvCustomDBDatePickerEdit.WMUndo(var Msg: TMessage);
begin
  if EditCanModify then
    inherited;
end;

function TJvCustomDBDatePickerEdit.EditCanModify: Boolean;
begin
  Result := (FLockEditing = 0) and not ReadOnly and (not IsLinked or FDataLink.Edit);
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

function TJvCustomDBDatePickerEdit.GetEnableValidation: Boolean;
begin
  Result := inherited GetEnableValidation;
  {if we enabled "as-you-type" validation for an unlinked control, we'd have
   validation errors pop up just from tabbing over the control, therefore we
   temporary disable it}
  if InternalChanging or Leaving then
    Result := Result and IsLinked and FDataLink.Editing;
end;

function TJvCustomDBDatePickerEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TJvCustomDBDatePickerEdit.IsEmpty: Boolean;
begin
  if IsLinked then
  begin
    if Assigned(FDataLink.DataSet) and FDataLink.DataSet.Active then
    begin
      if FDataLink.Editing or (FDataLink.Field = nil) then
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
  end
  else
    Result := AllowNoDate and (Date = NoDateValue);
end;

function TJvCustomDBDatePickerEdit.IsLinked: Boolean;
begin
  Result := Assigned(FDataLink) and Assigned(DataSource) and (DataField <> '');
end;

procedure TJvCustomDBDatePickerEdit.PopupDropDown(DisableEdit: Boolean);
begin
  if AllowPopupBrowsing or EditCanModify then
    inherited PopupDropDown(DisableEdit);
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

procedure TJvCustomDBDatePickerEdit.SetShowCheckbox(const AValue: Boolean);
begin
  inherited SetShowCheckbox(ValidateShowCheckBox(AValue));
end;

procedure TJvCustomDBDatePickerEdit.UpdateData(Sender: TObject);
begin
  if IsLinked and FDataLink.Editing then
  begin
    if not Checked or (AllowNoDate and ((Text = NoDateText) or IsEmptyMaskText(Text))) then
      FDataLink.Field.Clear
    else
      FDataLink.Field.AsDateTime := Self.Date;
  end;
end;

procedure TJvCustomDBDatePickerEdit.UpdateDisplay;
begin
  if IsLinked or not (csDesigning in ComponentState) then
  begin
    inherited UpdateDisplay;
  end
  else
  if not InternalChanging then    // Mantis 4093: Avoid stack overflow as setting Checked might call UpdateDisplay
  begin
    BeginInternalChange;
    try
      Checked := False;
      if not (csDesigning in ComponentState) then
        Text := '';
    finally
      EndInternalChange;
    end;
  end;
end;

function TJvCustomDBDatePickerEdit.ValidateDate(const ADate: TDateTime): Boolean;
begin
  Result := (not IsLinked or FDataLink.Active) or (FDataLink.DataSet.IsEmpty) or
    (not FDataLink.Editing) or
    ((not Focused) and (FDataLink.DataSet.State = dsInsert) and FDataLink.Field.IsNull) or
    (inherited ValidateDate(ADate));
end;

function TJvCustomDBDatePickerEdit.ValidateShowCheckBox(const AValue: Boolean): Boolean;
begin
  Result := AValue;
  if EnforceRequired and IsLinked and FDataLink.Active then
  begin
    if AValue and FDataLink.Field.Required then
      Result := False;
    AllowNoDate := not FDataLink.Field.Required;
  end;
end;

procedure TJvCustomDBDatePickerEdit.ValidateShowCheckBox;
begin
  inherited SetShowCheckbox(ValidateShowCheckBox(ShowCheckBox));
end;

procedure TJvCustomDBDatePickerEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key = #27) and not ReadOnly and Modified and IsLinked then
  begin
    FDataLink.Reset;
    Key := #0;
  end;
end;

function TJvCustomDBDatePickerEdit.GetInternalDate: TDateTime;
begin
  Result := Self.Date;
end;

procedure TJvCustomDBDatePickerEdit.SetInternalDate(const Value: TDateTime);
begin
  Inc(FLockEditing);
  try
    Self.Date := Value;
  finally
    Dec(FLockEditing);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.