{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPropAutoSave.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvAutoSave;

interface

uses
  SysUtils, Classes,
  Registry;

type
  TJvRegAutoSave = class(TPersistent)
  private
    FPath: string;
    FKey: string;
  published
    property Key: string read FKey write FKey;
    property Path: string read FPath write FPath;
  end;

  TJvAutoSave = class(TPersistent)
  private
    FAutoSave: Boolean;
    FRegistry: TJvRegAutoSave;
    FParent: TComponent;
  protected
    function CanLoadSave: Boolean;
  public
    constructor Create(Parent: TComponent);
    destructor Destroy; override;
    procedure SaveValue(Value: Integer); overload;
    procedure SaveValue(Value: Double); overload;
    procedure SaveValue(Value: Boolean); overload;
    procedure SaveValue(Value: string); overload;
    function LoadValue(var Default: Integer): Boolean; overload;
    function LoadValue(var Default: Double): Boolean; overload;
    function LoadValue(var Default: Boolean): Boolean; overload;
    function LoadValue(var Default: string): Boolean; overload;
  published
    property Active: Boolean read FAutoSave write FAutoSave default False;
    property Registry: TJvRegAutoSave read FRegistry write FRegistry;
  end;

implementation

constructor TJvAutoSave.Create(Parent: TComponent);
begin
  inherited Create;
  FAutoSave := False;
  FRegistry := TJvRegAutoSave.Create;
  FParent := Parent;
end;

destructor TJvAutoSave.Destroy;
begin
  FRegistry.Free;
  inherited Destroy;
end;

function TJvAutoSave.CanLoadSave: Boolean;
begin
  Result := False;
  if not (csDesigning in FParent.ComponentState) then
    if (FAutoSave) and (Registry.Key <> '') and (Registry.Path <> '') then
      Result := True;
end;

procedure TJvAutoSave.SaveValue(Value: Double);
begin
  if CanLoadSave then
    with TRegistry.Create do
    begin
      OpenKey(Registry.Path, True);
      WriteFloat(Registry.Key, Value);
      Free;
    end;
end;

procedure TJvAutoSave.SaveValue(Value: Integer);
begin
  if CanLoadSave then
    with TRegistry.Create do
    begin
      OpenKey(Registry.Path, True);
      WriteInteger(Registry.Key, Value);
      Free;
    end;
end;

function TJvAutoSave.LoadValue(var Default: Integer): Boolean;
begin
  Result := False;
  if CanLoadSave then
    with TRegistry.Create do
    begin
      OpenKey(Registry.Path, True);
      if ValueExists(Registry.Key) then
      try
        Default := ReadInteger(Registry.Key);
        Result := True;
      except
      end;
      Free;
    end;
end;

function TJvAutoSave.LoadValue(var Default: Double): Boolean;
begin
  Result := False;
  if CanLoadSave then
    with TRegistry.Create do
    begin
      OpenKey(Registry.Path, True);
      if ValueExists(Registry.Key) then
      try
        Default := ReadFloat(Registry.Key);
        Result := True;
      except
      end;
      Free;
    end;
end;

function TJvAutoSave.LoadValue(var Default: Boolean): Boolean;
begin
  Result := False;
  if CanLoadSave then
    with TRegistry.Create do
    begin
      OpenKey(Registry.Path, True);
      if ValueExists(Registry.Key) then
      try
        Default := ReadBool(Registry.Key);
        Result := True;
      except
      end;
      Free;
    end;
end;

function TJvAutoSave.LoadValue(var Default: string): Boolean;
begin
  Result := False;
  if CanLoadSave then
    with TRegistry.Create do
    begin
      OpenKey(Registry.Path, True);
      if ValueExists(Registry.Key) then
      try
        Default := ReadString(Registry.Key);
        Result := True;
      except
      end;
      Free;
    end;
end;

procedure TJvAutoSave.SaveValue(Value: string);
begin
  if CanLoadSave then
    with TRegistry.Create do
    begin
      OpenKey(Registry.Path, True);
      WriteString(Registry.Key, Value);
      Free;
    end;
end;

procedure TJvAutoSave.SaveValue(Value: Boolean);
begin
  if CanLoadSave then
    with TRegistry.Create do
    begin
      OpenKey(Registry.Path, True);
      WriteBool(Registry.Key, Value);
      Free;
    end;
end;

end.

