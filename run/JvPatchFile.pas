{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPatchFile.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPatchFile;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  JvTypes, JvComponent;

type
  TJvPatchFile = class(TJvComponent)
  private
    FEndFile: TFileName;
    FStartFile: TFileName;
    FDifferences: TStringList;
    FChangeInFile: Boolean;
    FPos: Integer;
    FPass: string;
    function Decrypt(Value: Byte): Byte;
    function GetDifferences: TStrings;
    procedure SetDifferences(Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    function Patch(Password: string = ''): Boolean;
    function IsPatched(FileName: string): Boolean;
    function IsPatchable(FileName: string): Boolean;
    property StartFile: TFileName read FStartFile write FStartFile;
    property EndFile: TFileName read FEndFile write FEndFile;
    property ChangeInFile: Boolean read FChangeInFile write FChangeInFile default True;
    property Differences: TStrings read GetDifferences write SetDifferences;
  end;

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

constructor TJvPatchFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDifferences := TStringList.Create;
  FChangeInFile := True;
end;

destructor TJvPatchFile.Destroy;
begin
  FDifferences.Free;
  inherited Destroy;
end;

function TJvPatchFile.Decrypt(Value: Byte): Byte;
begin
  if FPass = '' then
    Result := Value
  else
  begin
    FPos := (FPos + 1) mod Length(FPass);
    Result := Value xor Byte(FPass[FPos + 1]);
  end;
end;

function TJvPatchFile.GetDifferences: TStrings;
begin
  Result := FDifferences;
end;

procedure TJvPatchFile.SetDifferences(Value: TStrings);
begin
  FDifferences.Assign(Value);
end;

function TJvPatchFile.IsPatchable(FileName: string): Boolean;
var
  F: file of Byte;
begin
  Result := False;
  if FileExists(FileName) then
  begin
    AssignFile(F, FileName);
    Reset(F);
    Result := (FDifferences.Count > 3) and (FileSize(F) = StrToInt(FDifferences[2]));
    CloseFile(F);
  end;
end;

function TJvPatchFile.IsPatched(FileName: string): Boolean;
var
  F: file of Byte;
begin
  Result := False;
  if FileExists(FileName) then
  begin
    AssignFile(F, FileName);
    Reset(F);
    Result := (FDifferences.Count > 3) and (FileSize(F) = StrToInt(FDifferences[3]));
    CloseFile(F);
  end;
end;

function TJvPatchFile.Patch(Password: string): Boolean;
var
  I: Integer;
  Ind, Tmp: Longint;
  C: Byte;
  Bytes: array [0..65000] of Byte;
  T, EndT: TFileStream;
  T2: TMemoryStream;
begin
  FPos := -1;
  FPass := Password;

  //patch it !:)
  Result := False;
  if (FDifferences.Count = 0) or (FStartFile = '') or not FileExists(FStartFile) then
    Exit;

  T := TFileStream.Create(FStartFile, fmOpenRead or fmShareDenyWrite);
  T2 := TMemoryStream.Create;

  if (FDifferences.Count > 3) and (T.Size = StrToInt(FDifferences[2])) then
  begin
    Result := True;
    I := 4;
    while I < FDifferences.Count do
    begin
      if (Length(FDifferences[I]) > 2) and (Pos('|', FDifferences[I]) <> 0) then
      begin
        Ind := StrToInt(Copy(FDifferences[I], 1, Pos('|', FDifferences[I]) - 1));
        while Ind > 65000 do
        begin
          T.Read(Bytes, 60000);
          T2.Write(Bytes, 60000);
          Dec(Ind, 60000);
        end;
        T.Read(Bytes, Ind - 1);
        T.Read(C, 1);
        T2.Write(Bytes, Ind - 1);
        C := Byte(FDifferences[I][Pos('|', FDifferences[I]) + 1]);
        C := Decrypt(C);
        T2.Write(C, 1);
      end
      else
      if Length(FDifferences[I]) = 1 then
      begin
        //File is greater
        T.Position := T2.Position;
        while T.Position < T.Size do
        begin
          Ind := T.Read(Bytes, 10000);
          T2.Write(Bytes, Ind);
        end;
        C := Byte(FDifferences[I][1]);
        C := Decrypt(C);
        T2.Write(C, 1);
      end
      else
      if Pos('%', FDifferences[I]) = 4 then
      begin
        //File is smaller
        Ind := StrToInt(Copy(FDifferences[I], Pos('%', FDifferences[I]) + 1, Length(FDifferences[I])));
        while T.Position < Ind do
        begin
          Tmp := T.Read(Bytes, 10000);
          if Tmp + T.Position > Ind then
            T2.Write(Bytes, T2.Position - Ind)
          else
            T2.Write(Bytes, Tmp);
        end;
      end;
      Inc(I);
    end;
    T.Free;
    if FChangeInFile then
      EndT := TFileStream.Create(FStartFile, fmCreate or fmShareExclusive)
    else
      EndT := TFileStream.Create(FEndFile, fmCreate or fmShareExclusive);
    T2.Position := 0;
    EndT.CopyFrom(T2, T2.Size);
    EndT.Free;
  end
  else
    T.Free;
  T2.Free;
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

