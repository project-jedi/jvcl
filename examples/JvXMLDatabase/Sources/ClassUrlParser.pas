{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit ClassUrlParser;

interface

uses
  Windows, SysUtils, Classes, Contnrs;

type
  TUrlVariable = class
    Name, Value: string;
  end;

  TUrlParser = class
  private
    FVariables: TObjectList;
    function GetCount: Integer;
    function GetVariable(const AIndex: Integer): TUrlVariable;
  public
    constructor Create(AUrl: string);
    destructor Destroy;override;
    procedure AddVariable(AName, AValue: string);
    procedure DelVariable(AName: string);

    property Count: Integer read GetCount;
    property Variable[const AIndex: Integer]: TUrlVariable read GetVariable;
  end;

implementation

{ TUrlParser }

{**********************************************************************}
procedure TUrlParser.AddVariable(AName, AValue: string);
var
 lVar: TUrlVariable;
begin
  DelVariable(AName);
  lVar := TUrlVariable.Create;
  lVar.Name := ANAme;
  lVar.Value := AValue;
  FVariables.Add(lVar);
end;
{**********************************************************************}
constructor TUrlParser.Create(AUrl: string);
var
 i, j: Integer;
 lName, lValue, lMulti, st, st2, st3: string;

  procedure AddVariable;
  var
   lVar: TUrlVariable;
  begin
    lVar := TUrlVariable.Create;
    lVar.Name := lName;
    lVar.Value := lValue;
    lName := '';
    lValue := '';
    j := 0;
    FVariables.Add(lVar);
  end;

  function GetTempFile: string;
  var
    Buf: array[0..MAX_PATH] of char;
    Temp: array[0..MAX_PATH] of char;
  begin
    GetTempPath(MAX_PATH, Buf);
    GetTempFilename(Buf, 'tmp', 0, Temp);
    Result := String(Temp);
  end;

begin
  FVariables := TObjectList.Create;
  i := 1;
  while (i < Length(AUrl)) do
  begin
    if AUrl[i] = '?' then
      Break;
    inc(i);
  end;
  inc(i);

  j := 0;
  lName := '';
  lValue := '';
  lMulti := '';
  while i <= Length(AUrl) do
  begin
    case AUrl[i] of
      #10, #13:
        begin
          if (lName <> '') and (lName[1] = '-') then
          begin
            if lMulti = '' then
              lMulti := lName
            else if lMulti <> lName then
              Break;
            lName := '';
          end
          else if (lName <> '') or (lValue <> '') then
          begin
            if pos('Content-Disposition', lName) = 1 then
            begin
              //find out name and filename
              //Content-Disposition: form-data; name="plop"; filename="D:\Download\zion.jpeg"
              j := pos('name="', lName);
              if j <> 0 then
              begin
                st := Copy(lName, j + 6, MAXINT);
                j := pos('"', st);
                if j <> 0 then
                begin
                  lName := Copy(st, 1, j-1);
                  st := Copy(st, j + 1, MAXINT);
                  j := pos('filename="', st);
                  if j<>0 then
                  begin
                    //This is a file
                    st2 := lName;
                    lValue := Copy(st, j + 10, MAXINT);
                    j := pos('"', lValue);
                    SetLength(lValue, j - 1);
                    lName := 'UPLOAD_FILENAME';
                    st3 := lValue;
                    AddVariable;

                    lValue := ExtractFileName(st3);
                    lName := 'UPLOAD_BASENAME';
                    AddVariable;

                    lValue := '';
                    lName := st2;
                  end;
                end;
              end;

              j := 1;
            end
            else if pos('Content-Type', lValue) = 1 then
            begin
              //This is a file
              while AUrl[i] in [#10, #13] do
                inc(i);

              //Read the file!
              lValue := GetTempFile;
              with TFileStream.Create(lValue, fmCreate) do
              try
                j := pos(lMulti, AUrl);
                Write(AUrl[i], (j - i) - 2);
                i := j;
              finally
                Free;
              end;
              AddVariable;
            end
            else if (lName <> '') then
              AddVariable;
          end;
          while AUrl[i] in [#10, #13] do
            inc(i);
          dec(i);
        end;
      '&':
        AddVariable;
      '=':
        if lMulti = '' then
          j := 1
        else
          lName := lName + '=';
      else
        if j = 0 then
        begin
          lName := lName + AUrl[i];
          AUrl[i] := ' ';
        end
        else
          lValue := lValue + AUrl[i];
    end;
    inc(i);
  end;

  if lName<>'' then
    AddVariable;
end;
{**********************************************************************}
procedure TUrlParser.DelVariable(AName: string);
var
 i: Integer;
begin
  for i:=FVariables.Count-1 downto 0 do
    if TUrlVariable(FVariables[i]).Name = AName then
      FVariables.Delete(i);
end;
{**********************************************************************}
destructor TUrlParser.Destroy;
begin
  FVariables.Free;
  inherited;
end;
{**********************************************************************}
function TUrlParser.GetCount: Integer;
begin
  result := FVariables.Count;
end;
{**********************************************************************}
function TUrlParser.GetVariable(const AIndex: Integer): TUrlVariable;
begin
  result := TUrlVariable(FVariables[AIndex]);
end;
{**********************************************************************}
end.
