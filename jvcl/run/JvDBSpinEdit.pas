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
  EinWill

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org
Known Issues:

-----------------------------------------------------------------------------}
// $Id$

unit JvDBSpinEdit;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Controls, DB, DBCtrls,
  JvSpin, JvConsts;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvDBSpinEdit = class(TJvSpinEdit)
  private
    FDataLink: TFieldDataLink;
    FIsNull: Boolean;
    FAllowNull: Boolean;
    FDataChanging: Boolean;

    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure EditingChange(Sender: TObject);
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

    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property IsNull: Boolean read FIsNull;
  published
    property AllowNull: Boolean read FAllowNull write FAllowNull default True;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
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

//=== { TJvDBSpinEdit } ======================================================

constructor TJvDBSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  FAllowNull := True;
  FDataChanging := False;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TJvDBSpinEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TJvDBSpinEdit.Change;
begin
  if (FDataLink <> nil) and (FDataLink.Field <> nil) then
    FDataLink.Modified;
  inherited Change;
end;

procedure TJvDBSpinEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := LRESULT(FDataLink);
end;

procedure TJvDBSpinEdit.DataChange(Sender: TObject);
begin
  FDataChanging := True;
  try
    if FDataLink.Field <> nil then
    begin
      if Focused and FDataLink.CanModify then
      begin
        // Mantis 2131: If field is numeric and it has a DisplayFormat then
        // take the unformatted text (in AsString) to have a valid number in
        // the Value property.
        if (FDataLink.Field is TNumericField) and
          (Length((FDataLink.Field as TNumericField).DisplayFormat) <> 0) then
          Text := FDataLink.Field.AsString
        else
          Text := FDataLink.Field.Text;
      end
      else
      begin
        FIsNull := FDataLink.Field.DisplayText = '';
        // Mantis 2131, see above
        if (FDataLink.Field is TNumericField) and
          (Length((FDataLink.Field as TNumericField).DisplayFormat) <> 0) then
          Text := FDataLink.Field.AsString
        else
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
  finally
    FDataChanging := False;
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
    Value := 0;
    FIsNull := False;
    Text := '0';
  end;
  inherited DownClick(Sender);
end;

procedure TJvDBSpinEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

function TJvDBSpinEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or
    (Assigned(FDataLink) and FDataLink.ExecuteAction(Action));
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
  FIsNull := (Text = '');
  if FIsNull then
    Result := 0.0
  else
    Result := inherited GetValue;
end;

function TJvDBSpinEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := inherited IsValidChar(Key);
  if not Result and AllowNull and
    ((Key = BackSpace) or (Key = Del)) then
    Result := True;
end;

procedure TJvDBSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  KeyState: TKeyboardState;
  AnsiChars: AnsiString;
begin
  if (Key = VK_ESCAPE) and (FDataLink.Editing) then
  begin
    FDataLink.Reset;
    SelectAll;
    Key := 0;
  end;
  inherited KeyDown(Key, Shift);

  // Must convert from virtual key code to character
  GetKeyboardState(KeyState);
  SetLength(AnsiChars, 2);
  case ToAscii(Key, MapVirtualKey(Key, 0), KeyState, @AnsiChars[1], 0) of
    1: SetLength(AnsiChars, 1);
    2: ;
    else AnsiChars := '';
  end;

  if (Key = VK_DELETE) or (Key = VK_BACK) or
    ((Key = VK_INSERT) and (ssShift in Shift)) or ((Length(AnsiChars) > 0) and IsValidChar(Char(AnsiChars[1]))) then
    FDataLink.Edit;
end;

procedure TJvDBSpinEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
    if Assigned(FDataLink) and (AComponent = DataSource) then
      DataSource := nil;
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
  begin
    if FDataLink.DataSource <> nil then
      FDataLink.DataSource.RemoveFreeNotification(Self);
    FDataLink.DataSource := Value;
  end;
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
  if FDataLink.CanModify then
  begin
    FIsNull := (Text = '') and (NewValue = 0.0);
    if not (FIsNull and FAllowNull) then
      inherited SetValue(NewValue);
    if not FDataChanging and (FDataLink.Field <> nil) then
    begin
      if (IsNull and not FDataLink.Field.IsNull) or
         (not IsNull and not VarSameValue(FDataLink.Field.Value, NewValue)) then
        FDataLink.Edit;
    end;
  end;
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
    Value := 0;
    FIsNull := False;
    Text := '0';
  end
  else
    inherited UpClick(Sender);
end;

{ UpdateData is only called after calls to both FDataLink.Modified and
  FDataLink.UpdateRecord. }

function TJvDBSpinEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or
    (Assigned(FDataLink) and FDataLink.UpdateAction(Action));
end;

procedure TJvDBSpinEdit.UpdateData(Sender: TObject);
begin
  { Never masked? }
  {ValidateEdit;}
  if FDataLink.Editing then
    FDataLink.Field.AsFloat := Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
