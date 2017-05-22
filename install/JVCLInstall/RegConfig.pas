{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RegConfig.pas, released on 2007-12-15.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2007 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit RegConfig;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Registry;

type
  TJVCLRegistryConfig = class(TObject)
  private
    FItems: TStrings;
    FModified: Boolean;
    FDeletedItems: TStrings;
    function GetCount: Integer;
    function GetEnabled(const Index: string): Boolean;
    function GetName(Index: Integer): string;
    procedure SetEnabled(const Index: string; const Value: Boolean); // Objects[]=>Boolean
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromRegistry(const Key: string);
    procedure SaveToRegistry(const Key: string);
    function Contains(const Name: string): Boolean;
    procedure Remove(const Name: string);
    procedure Delete(Index: Integer);

    property Count: Integer read GetCount;
    property Names[Index: Integer]: string read GetName;

    property Enabled[const Index: string]: Boolean read GetEnabled write SetEnabled; default;
    property Modified: Boolean read FModified write FModified;
  end;


implementation

{ TJVCLRegistryConfig }

constructor TJVCLRegistryConfig.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
  FDeletedItems := TStringList.Create;
end;

destructor TJVCLRegistryConfig.Destroy;
begin
  FDeletedItems.Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TJVCLRegistryConfig.Delete(Index: Integer);
begin
  if Index <> -1 then
  begin
    FDeletedItems.Add(FItems[Index]);
    FItems.Delete(Index);
    FModified := True;
  end;
end;

procedure TJVCLRegistryConfig.Remove(const Name: string);
begin
  Delete(FItems.IndexOf(Name));
end;

function TJVCLRegistryConfig.Contains(const Name: string): Boolean;
begin
  Result := FItems.IndexOf(Name) <> -1;
end;

function TJVCLRegistryConfig.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJVCLRegistryConfig.GetEnabled(const Index: string): Boolean;
var
  I: Integer;
begin
  I := FItems.IndexOf(Index);
  if I <> -1 then
    Result := Boolean(FItems.Objects[I])
  else
    Result := False;
end;

procedure TJVCLRegistryConfig.SetEnabled(const Index: string; const Value: Boolean);
var
  I: Integer;
begin
  I := FItems.IndexOf(Index);
  if I <> -1 then
  begin
    if Value <> Enabled[Index] then
    begin
      FItems.Objects[I] := TObject(Value);
      FModified := True;
    end;
  end
  else
  begin
    I := FDeletedItems.IndexOf(Index);
    if I <> -1 then
      FDeletedItems.Delete(I);
    FItems.AddObject(Index, TObject(Value));
    FModified := True;
  end;
end;

function TJVCLRegistryConfig.GetName(Index: Integer): string;
begin
  Result := FItems[Index];
end;

procedure TJVCLRegistryConfig.LoadFromRegistry(const Key: string);
var
  Reg: TRegistry;
  I: Integer;
begin
  FDeletedItems.Clear;
  FItems.Clear;
  Reg := TRegistry.Create;
  try
    if Reg.OpenKeyReadOnly(Key + '\IDE') then
    begin
      Reg.GetValueNames(FItems);
      for I := 0 to FItems.Count - 1 do
        FItems.Objects[I] := TObject(Reg.ReadBool(FItems[I]));
    end;
  finally
    Reg.Free;
  end;
  FModified := False;
end;

procedure TJVCLRegistryConfig.SaveToRegistry(const Key: string);
var
  Reg: TRegistry;
  I: Integer;
begin
  if Modified then
  begin
    Reg := TRegistry.Create;
    try
      if Reg.OpenKey(Key + '\IDE', True) then
      begin
        for I := 0 to FDeletedItems.Count - 1 do
          Reg.DeleteValue(FDeletedItems[I]);

        for I := 0 to FItems.Count - 1 do
          Reg.WriteBool(FItems[I], Boolean(FItems.Objects[I]));
      end;
    finally
      Reg.Free;
    end;
    FDeletedItems.Clear;
    FModified := False;
  end;
end;

end.