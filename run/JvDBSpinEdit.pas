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

{$I jvcl.inc}

unit JvDBSpinEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, DB, DBCtrls,
  JvSpin;

type
  TJvDBSpinEdit = class(TJvSpinEdit)
  private
    FFieldDataLink: TFieldDataLink;
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
    procedure SetDataSource(NewSource: TDataSource); { Assigns new data source. }
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
    function GetReadOnlyField: Boolean;
    procedure SetReadOnlyField(Value: Boolean);
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IsNull: Boolean read FIsNull;
  published
    property AllowNull: Boolean read FAllowNull write FAllowNull default True;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnlyField: Boolean read GetReadOnlyField write SetReadOnlyField;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$IFDEF COMPILER6_UP}
uses
  Variants;
{$ENDIF COMPILER6_UP}

constructor TJvDBSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowNull := True;
  FFieldDataLink := TFieldDataLink.Create;
  FFieldDataLink.Control := Self;
  FFieldDataLink.OnDataChange := DataChange;
  FFieldDataLink.OnEditingChange := EditingChange;
  FFieldDataLink.OnUpdateData := UpdateData;
  FFieldDataLink.OnActiveChange := ActiveChange;
end;

destructor TJvDBSpinEdit.Destroy;
begin
  FFieldDataLink.Free;
  inherited Destroy;
end;

procedure TJvDBSpinEdit.DoExit;
begin
  try
    if Modified then
      FFieldDataLink.Modified;
    FFieldDataLink.UpdateRecord; { tell data link to update database }
  except
    SetFocus; { if it failed, don't let focus leave }
    SelectAll;
    raise;
  end;
  inherited DoExit;
end;

{ UpdateData is only called after calls to both FFieldDataLink.Modified and
  FFieldDataLink.UpdateRecord. }

procedure TJvDBSpinEdit.UpdateData(Sender: TObject);
begin
  if FFieldDataLink.Editing then
    FFieldDataLink.Field.Text := Text;
end;

procedure TJvDBSpinEdit.DataChange(Sender: TObject); { Triggered when data changes in DataSource. }
begin
  if FFieldDataLink.Field <> nil then
  begin
    if Focused and FFieldDataLink.CanModify then
      Text := FFieldDataLink.Field.Text
    else
    begin
      FIsNull := (FFieldDataLink.Field.DisplayText = '');
      Text := FFieldDataLink.Field.DisplayText;
      if FFieldDataLink.Editing or (FFieldDataLink.Field.DataSet.State = dsInsert) then
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

function TJvDBSpinEdit.GetDataField: string; { Returns data field name. }
begin
  { FFieldDataLink is built in TJvDBSpinEdit.Create; there's no need to check to see if it's assigned. }
  Result := FFieldDataLink.FieldName;
end;

function TJvDBSpinEdit.GetDataSource: TDataSource; { Returns linked data source. }
begin
  { FFieldDataLink is built in TJvDBSpinEdit.Create; there's no need to check to see if it's assigned. }
  Result := FFieldDataLink.DataSource;
end;

procedure TJvDBSpinEdit.SetDataField(const NewFieldName: string); { Assigns new field. }
begin
  { FFieldDataLink is built in TJvDBSpinEdit.Create; there's no need to check to see if it's assigned. }
  FFieldDataLink.FieldName := NewFieldName;
end;

procedure TJvDBSpinEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Longint(FFieldDataLink);
end;

procedure TJvDBSpinEdit.SetDataSource(NewSource: TDataSource); { Assigns new data source. }
begin
  { FFieldDataLink is built in TJvDBSpinEdit.Create; there's no need to check to see if it's assigned. }
  FFieldDataLink.DataSource := NewSource;
  { Tell the new DataSource that our TJvDBSpinEdit component should be notified
    (using the Notification method) if the DataSource is ever removed from
    a data module or form that is different than the owner of this control. }
  if NewSource <> nil then
    NewSource.FreeNotification(Self);
end;

function TJvDBSpinEdit.GetReadOnlyField: Boolean;
begin
  Result := FFieldDataLink.ReadOnly;
end;

procedure TJvDBSpinEdit.SetReadOnlyField(Value: Boolean);
begin
  FFieldDataLink.ReadOnly := Value;
end;

procedure TJvDBSpinEdit.Change;
begin
  if (FFieldDataLink <> nil) and (FFieldDataLink.Field <> nil) then
    FFieldDataLink.Modified;
  inherited Change;
end;

function TJvDBSpinEdit.GetValue: Extended;
begin
  Result := inherited GetValue;
  FIsNull := (Text = '') and (Result = 0.0);
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

procedure TJvDBSpinEdit.DownClick(Sender: TObject);
begin
  FFieldDataLink.Edit;
  if IsNull then
  begin
    FIsNull := False;
    Value := 1;
    FIsNull := False;
    Text := '1';
  end;
  inherited DownClick(Sender);
end;

procedure TJvDBSpinEdit.UpClick(Sender: TObject);
begin
  FFieldDataLink.Edit;
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

procedure TJvDBSpinEdit.ActiveChange(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
    Enabled := FFieldDataLink.Active;
end;

procedure TJvDBSpinEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FFieldDataLink.Editing;
end;

procedure TJvDBSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or (Key = VK_BACK) or
    ((Key = VK_INSERT) and (ssShift in Shift)) or IsValidChar(Char(Key)) then
    FFieldDataLink.Edit;
end;

function TJvDBSpinEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := inherited IsValidChar(Key);
  if not Result and AllowNull and
    (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE)) then
    Result := True;
end;

end.

