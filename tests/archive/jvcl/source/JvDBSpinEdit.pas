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

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net
Known Issues:

-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDBSpinEdit;

interface

{$DEFINE READONLY}

//  RegisterComponents(cJvDataControlsPalette, [TJvDBSpinEdit]);

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, DB, DBCtrls,
  JvSpin;

type
  TJvDBSpinEdit = class(TJvSpinEdit)
  private
    {$IFDEF READONLY}
    FReadOnly: Boolean;
    {$ENDIF}
    FFieldDataLink: TFieldDataLink;
    FOnChange: TNotifyEvent;
    procedure DataChange(Sender: TObject); { Triggered when data changes in DataSource. }
    procedure UpdateData(Sender: TObject); { Triggered when data in control changes (via FFieldDataLink.UpdateRecord). }
    function GetDataField: string; { Returns data field name. }
    function GetDataSource: TDataSource; { Returns linked data source. }
    procedure SetDataField(const NewFieldName: string); { Assigns new field. }
    procedure SetDataSource(NewSource: TDataSource); { Assigns new data source. }
    procedure CMExit(var Msg: TCMExit); message CM_EXIT; { called to update data }
    {$IFDEF WIN32}
    procedure CMGetDataLink(var Msg: TMessage); message CM_GetDataLink;
    procedure SetReadOnly(Value: Boolean);
    procedure SetOnChange(const Value: TNotifyEvent);
    {$ENDIF}
  protected
    procedure DoChange(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    {$IFDEF READONLY}
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    {$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation

constructor TJvDBSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFieldDataLink := TFieldDataLink.Create;
  FFieldDataLink.Control := Self;
  FFieldDataLink.OnDataChange := DataChange; { So we can respond to changes in data. }
  FFieldDataLink.OnUpdateData := UpdateData; { So data in linked table is updated when user edits control. }
  inherited OnChange := Self.DoChange;
end;

destructor TJvDBSpinEdit.Destroy;
begin
  FFieldDataLink.Free;
  inherited Destroy;
end;

procedure TJvDBSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
{ Only process the keyboard input if it is cursor motion or if the data
  link can edit the data. Otherwise, call the OnKeyDown event handler
  (if it's assigned). }
var
  KeyDownEventHandler: TKeyEvent;
begin
  if ((not ReadOnly) and FFieldDataLink.Edit) or (Key in [VK_UP, VK_DOWN, VK_LEFT,
    VK_RIGHT, VK_END, VK_HOME, VK_PRIOR, VK_NEXT]) then
    inherited KeyDown(Key, Shift)
  else
  begin { Our responsibility to call OnKeyDown if it's assigned, as we're skipping inherited method. }
    KeyDownEventHandler := OnKeyDown;
    if Assigned(KeyDownEventHandler) then
      KeyDownEventHandler(Self, Key, Shift);
  end;
end;

procedure TJvDBSpinEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{ Only process mouse messages if the data link can edit the data. Otherwise,
  call the OnMouseDown event handler (if it's assigned). }
var
  MouseDownEventHandler: TMouseEvent;
begin
  if (not ReadOnly) and FFieldDataLink.Edit then { OK to edit. }
    inherited MouseDown(Button, Shift, X, Y)
  else
  begin { Our responsibility to call OnMouseDown if it's assigned, as we're skipping inherited method. }
    MouseDownEventHandler := OnMouseDown;
    if Assigned(MouseDownEventHandler) then
      MouseDownEventHandler(Self, Button, Shift, X, Y);
  end;
end;

procedure TJvDBSpinEdit.DoChange(Sender: TObject);
begin
  if FFieldDataLink.Edit then
  begin
    FFieldDataLink.Modified; { Data has changed. }
    if Assigned(FOnChange) then
      FOnChange(self);
  end;
end;

procedure TJvDBSpinEdit.CMExit(var Msg: TCMExit);
begin
  try
    FFieldDataLink.UpdateRecord; { tell data link to update database }
  except
    SetFocus; { if it failed, don't let focus leave }
    raise;
  end;
  inherited;
end;

procedure TJvDBSpinEdit.UpdateData(Sender: TObject);
{ UpdateData is only called after calls to both FFieldDataLink.Modified and
  FFieldDataLink.UpdateRecord. }
begin
  if not ReadOnly then
    FFieldDataLink.Field.AsString := Self.Text;
end;

procedure TJvDBSpinEdit.DataChange(Sender: TObject); { Triggered when data changes in DataSource. }
begin
  if FFieldDataLink.Field = nil then
    Self.Text := ' '
  else
    Self.Text := FFieldDataLink.Field.AsString;
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

{$IFDEF WIN32}
procedure TJvDBSpinEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Longint(FFieldDataLink);
end;
{$ENDIF}

procedure TJvDBSpinEdit.SetDataSource(NewSource: TDataSource); { Assigns new data source. }
begin
  { FFieldDataLink is built in TJvDBSpinEdit.Create; there's no need to check to see if it's assigned. }
  FFieldDataLink.DataSource := NewSource;
  {$IFDEF WIN32}
  { Tell the new DataSource that our TJvDBSpinEdit component should be notified
    (using the Notification method) if the DataSource is ever removed from
    a data module or form that is different than the owner of this control. }
  if NewSource <> nil then
    NewSource.FreeNotification(Self);
  {$ENDIF}
end;

{$IFDEF READONLY}
procedure TJvDBSpinEdit.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    FFieldDataLink.ReadOnly := Value;
  end;

end;
{$ENDIF}

procedure TJvDBSpinEdit.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

end.

