{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSimPIDlinker.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQSimPIDLinker;

{$I jvcl.inc}

interface

uses
  Classes,
  JvQSimPID;

type
  TJvSimPIDLinker = class(TComponent)
  private
    FPIDS: array of TJvSimPID;
    function GetPID(const Index: Integer): TJvSimPID;
    procedure SetPID(const Index: Integer; const Value: TJvSimPID);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure InitPids;
  public
    procedure Execute;
    constructor Create(AOwner: TComponent); override;
  published
    property In1: TJvSimPID index 0 read GetPID write SetPID;
    property Out1: TJvSimPID index 1 read GetPID write SetPID;
    property In2: TJvSimPID index 2 read GetPID write SetPID;
    property Out2: TJvSimPID index 3 read GetPID write SetPID;
    property In3: TJvSimPID index 4 read GetPID write SetPID;
    property Out3: TJvSimPID index 5 read GetPID write SetPID;
    property In4: TJvSimPID index 6 read GetPID write SetPID;
    property Out4: TJvSimPID index 7 read GetPID write SetPID;
    property In5: TJvSimPID index 8 read GetPID write SetPID;
    property Out5: TJvSimPID index 9 read GetPID write SetPID;
    property In6: TJvSimPID index 10 read GetPID write SetPID;
    property Out6: TJvSimPID index 11 read GetPID write SetPID;
    property In7: TJvSimPID index 12 read GetPID write SetPID;
    property Out7: TJvSimPID index 13 read GetPID write SetPID;
    property In8: TJvSimPID index 14 read GetPID write SetPID;
    property Out8: TJvSimPID index 15 read GetPID write SetPID;
  end;

implementation

constructor TJvSimPIDLinker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitPids;
end;

procedure TJvSimPIDLinker.Execute;
var
  I: Integer;
begin
  for I := 0 to Length(FPIDS) - 2 do
    if (FPIDS[I] <> nil) and (FPIDS[I + 1] <> nil) then
      FPIDS[I].MV := FPIDS[I + 1].CV;
end;

function TJvSimPIDLinker.GetPID(const Index: Integer): TJvSimPID;
begin
  Result := FPIDS[Index];
end;

procedure TJvSimPIDLinker.InitPids;
const
  cCount = 16;
var
  I: Integer;
begin
  SetLength(FPIDS, cCount);
  for I := 0 to cCount - 1 do
    FPIDS[I] := nil;
end;

procedure TJvSimPIDLinker.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    for I := 0 to Length(FPIDS) - 1 do
      if FPIDS[I] = AComponent then
        FPIDS[I] := nil;
end;

procedure TJvSimPIDLinker.SetPID(const Index: Integer;
  const Value: TJvSimPID);
begin
  FPIDS[Index] := Value;
end;

end.

