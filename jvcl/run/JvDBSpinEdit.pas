{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBSpinEdit.PAS, released on 2002-07-26.

The Initial Developer of the Original Code is Rob den Braasem []
Portions created by Rob den Braasem are Copyright (C) 2002 Rob den Braasem.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net
Known Issues:

-----------------------------------------------------------------------------}
// $Id$

unit JvDBSpinEdit;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Classes, Controls, DB, DBCtrls,
  JvSpin;

type
  TJvDBSpinEdit = class(TJvSpinEdit)
  private
    FDataLink: TFieldDataLink;
    FOnChange: TNotifyEvent;
    FIsNull: Boolean;
    FAllowNull: Boolean;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    function GetDataField: string; { Returns data field name. }
    function GetDataSource: TDataSource; { Returns linked data source. }
    procedure SetDataField(const NewFieldName: string); { Assigns new field. }
    procedure SetDataSource(Value: TDataSource); { Assigns new data source. }
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
    function GetReadOnly: Boolean; reintroduce;
    procedure SetReadOnly(Value: Boolean); reintroduce;
  protected
    function IsValidChar(Key: Char): Boolean; override;
    procedure Change; override;
    procedure DoExit; override; { called to update data }
    function GetValue: Extended; override;
    procedure SetValue(NewValue: Extended); override;
    procedure TextChanged; override;
    procedure UpClick(Sender: TObject); override;
    procedure DownClick(Sender: TObject); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    { Backwards compatibility; eventually remove }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadReadOnlyField(Reader: TReader);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IsNull: Boolean read FIsNull;
  published
    { (rb) Property Value should be public and not stored. This can only be
           done by adjusting TJvSpinEdit/TJvCustomSpinEdit (?) }
    property AllowNull: Boolean read FAllowNull write FAllowNull default True;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  SysUtils;

//=== { TJvDBSpinEdit } ======================================================

constructor TJvDBSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  FAllowNull := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnActiveChange := ActiveChange;
end;

destructor TJvDBSpinEdit.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

procedure TJvDBSpinEdit.ActiveChange(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
    Enabled := FDataLink.Active;
end;

procedure TJvDBSpinEdit.Change;
begin
  if (FDataLink <> nil) and (FDataLink.Field <> nil) then
    FDataLink.Modified;
  inherited Change;
end;

procedure TJvDBSpinEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Longint(FDataLink);
end;

procedure TJvDBSpinEdit.DataChange(Sender: TObject); { Triggered when data changes in DataSource. }
begin
  if FDataLink.Field <> nil then
  begin
    if Focused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
    begin
      FIsNull := (FDataLink.Field.DisplayText = '');
      Text := FDataLink.Field.DisplayText;
      if FDataLink.Editing or (FDataLink.Field.DataSet.State = dsInsert) then
        Modified := True;
    end;
  end
  else
  begin
    FIsNull := False;
    if csDesigning in ComponentState then
      Text := Name
    else
      Text := '';
  end;
end;

procedure TJvDBSpinEdit.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ReadOnlyField', ReadReadOnlyField, nil, False);
end;

procedure TJvDBSpinEdit.DoExit;
begin
  try
    if Modified then
      FDataLink.Modified;
    FDataLink.UpdateRecord; { tell data link to update database }
  except
    SetFocus; { if it failed, don't let focus leave }
    SelectAll;
    raise;
  end;
  inherited DoExit;
end;

procedure TJvDBSpinEdit.DownClick(Sender: TObject);
begin
  FDataLink.Edit;
  if IsNull then
  begin
    FIsNull := False;
    Value := 1;
    FIsNull := False;
    Text := '1';
  end;
  inherited DownClick(Sender);
end;

procedure TJvDBSpinEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

function TJvDBSpinEdit.GetDataField: string; { Returns data field name. }
begin
  { FDataLink is built in TJvDBSpinEdit.Create; there's no need to check to see if it's assigned. }
  Result := FDataLink.FieldName;
end;

function TJvDBSpinEdit.GetDataSource: TDataSource; { Returns linked data source. }
begin
  { FDataLink is built in TJvDBSpinEdit.Create; there's no need to check to see if it's assigned. }
  Result := FDataLink.DataSource;
end;

function TJvDBSpinEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

function TJvDBSpinEdit.GetValue: Extended;
begin
  Result := inherited GetValue;
  FIsNull := (Text = '') and (Result = 0.0);
end;

function TJvDBSpinEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := inherited IsValidChar(Key);
  if not Result and AllowNull and
    (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE)) then
    Result := True;
end;

procedure TJvDBSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or (Key = VK_BACK) or
    ((Key = VK_INSERT) and (ssShift in Shift)) or IsValidChar(Char(Key)) then
    FDataLink.Edit;
end;

procedure TJvDBSpinEdit.ReadReadOnlyField(Reader: TReader);
begin
  ReadOnly := Reader.ReadBoolean;
end;

procedure TJvDBSpinEdit.SetDataField(const NewFieldName: string); { Assigns new field. }
begin
  { FDataLink is built in TJvDBSpinEdit.Create; there's no need to check to see if it's assigned. }
  FDataLink.FieldName := NewFieldName;
end;

procedure TJvDBSpinEdit.SetDataSource(Value: TDataSource); { Assigns new data source. }
begin
  { FDataLink is built in TJvDBSpinEdit.Create; there's no need to check to see if it's assigned. }
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  { Tell the new DataSource that our TJvDBSpinEdit component should be notified
    (using the Notification method) if the DataSource is ever removed from
    a data module or form that is different than the owner of this control. }
  if Value <> nil then
    Value.FreeNotification(Self);

  //FDataLink.DataSource := NewSource;
  //if NewSource <> nil then
  //  NewSource.FreeNotification(Self);
end;

procedure TJvDBSpinEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

procedure TJvDBSpinEdit.SetValue(NewValue: Extended);
begin
  FIsNull := (Text = '') and (NewValue = 0.0);
  inherited SetValue(NewValue);
end;

procedure TJvDBSpinEdit.TextChanged;
begin
  if FIsNull and AllowNull then
    inherited Text := ''
  else
    inherited TextChanged;
end;

procedure TJvDBSpinEdit.UpClick(Sender: TObject);
begin
  FDataLink.Edit;
  if IsNull then
  begin
    FIsNull := False;
    Value := 1;
    FIsNull := False;
    Text := '1';
  end
  else
    inherited UpClick(Sender);
end;

{ UpdateData is only called after calls to both FDataLink.Modified and
  FDataLink.UpdateRecord. }

procedure TJvDBSpinEdit.UpdateData(Sender: TObject);
begin
  { Never masked? }
  {ValidateEdit;}
  if FDataLink.Editing then
    FDataLink.Field.Text := Text;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
