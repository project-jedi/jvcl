{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPatchFile.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvPatchFile;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvTypes, JvComponent;

type
  TJvPatchFile = class(TJvComponent)
  private
    FEnd: TFileName;
    FStart: TFileName;
    FDiff: TStringList;
    FChange: Boolean;
    FPos: Integer;
    FPass: string;
    function Decrypt(Value: Byte): Byte;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property StartFile: TFileName read FStart write FStart;
    property EndFile: TFileName read Fend write FEnd;
    property ChangeInFile: Boolean read FChange write FChange default True;
    property Differences: TStringList read FDiff write FDiff;
    function Patch(Password: string = ''): Boolean;
    function IsPatched(FileName: string): Boolean;
    function IsPatchable(FileName: string): Boolean;
  end;

implementation

{**************************************************}

constructor TJvPatchFile.Create(AOwner: TComponent);
begin
  inherited;
  FDiff := TStringList.Create;
  FChange := True;
end;

{**************************************************}

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

{**************************************************}

destructor TJvPatchFile.Destroy;
begin
  FDiff.Free;
  inherited;
end;

{**************************************************}

function TJvPatchFile.IsPatchable(FileName: string): Boolean;
var
  f: file of Byte;
begin
  Result := False;
  if FileExists(FileName) then
  begin
    AssignFile(f, FileName);
    Reset(f);
    Result := (FDiff.Count > 3) and (FileSize(f) = StrToInt(FDiff[2]));
    CloseFile(f);
  end;
end;

{**************************************************}

function TJvPatchFile.IsPatched(FileName: string): Boolean;
var
  f: file of Byte;
begin
  Result := False;
  if FileExists(FileName) then
  begin
    AssignFile(f, FileName);
    Reset(f);
    Result := (FDiff.Count > 3) and (FileSize(f) = StrToInt(FDiff[3]));
    CloseFile(f);
  end;
end;

{**************************************************}

function TJvPatchFile.Patch(Password: string): Boolean;
var
  i: Integer;
  ind, tmp: longint;
  c: Byte;
  b: array[0..65000] of Byte;
  t, tend: TFileStream;
  t2: TMemoryStream;
begin
  FPos := -1;
  FPass := Password;

  //patch it !:)
  Result := False;
  if (FDiff.Count = 0) or (FStart = '') or not FileExists(FStart) then
    Exit;

  t := TFileStream.Create(FStart, fmOpenRead or fmShareDenyWrite);
  t2 := TMemoryStream.Create;

  if (FDiff.Count > 3) and (t.Size = StrToInt(FDiff[2])) then
  begin
    Result := True;
    i := 4;
    while i < FDiff.Count do
    begin
      if (Length(FDiff[i]) > 2) and (Pos('|', FDiff[i]) <> 0) then
      begin
        ind := StrToInt(Copy(FDiff[i], 1, Pos('|', FDiff[i]) - 1));
        while ind > 65000 do
        begin
          t.Read(b, 60000);
          t2.Write(b, 60000);
          Dec(ind, 60000);
        end;
        t.Read(b, ind - 1);
        t.Read(c, 1);
        t2.Write(b, ind - 1);
        c := Byte(FDiff[i][Pos('|', FDiff[i]) + 1]);
        c := Decrypt(c);
        t2.Write(c, 1);
      end
      else if Length(FDiff[i]) = 1 then
      begin
        //File is greater
        t.Position := t2.Position;
        while t.Position < t.Size do
        begin
          ind := t.Read(b, 10000);
          t2.Write(b, ind);
        end;
        c := Byte(FDiff[i][1]);
        c := Decrypt(c);
        t2.Write(c, 1);
      end
      else if (Pos('%', FDiff[i]) = 4) then
      begin
        //File is smaller
        ind := StrToInt(Copy(FDiff[i], Pos('%', FDiff[i]) + 1, Length(FDiff[i])));
        while t.Position < ind do
        begin
          tmp := t.Read(b, 10000);
          if tmp + t.position > ind then
            t2.Write(b, t2.position - ind)
          else
            t2.Write(b, tmp);
        end;
      end;
      Inc(i);
    end;
    t.Free;
    if FChange then
      tend := TFileStream.Create(FStart, fmCreate or fmShareExclusive)
    else
      tend := TFileStream.Create(FEnd, fmCreate or fmShareExclusive);
    t2.Position := 0;
    tend.CopyFrom(t2, t2.Size);
    tend.Free;
  end
  else
    t.Free;
  t2.Free;
end;

end.
