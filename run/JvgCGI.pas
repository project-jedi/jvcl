{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgCGI.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgCGI;

interface

// (rom) from the Delphi help
// The $APPTYPE directive is meaningful only in a program.
// It should not be used in a library, unit, or package.
{ APPTYPE CONSOLE}

uses
  Windows, SysUtils, Classes;

type
  TJvgCGI = class(TObject)
  private
    FAutoHeaderAndFooter: Boolean;
    procedure Split(S: string);
    procedure Decode(sl: TStringList);
  public
    //      Names: TStringList;
    //      Values: TStringList;
    Val: array [0..500] of Char;
    Params: TStringList;
    constructor Create(AutoHeaderAndFooter: Boolean);
    destructor Destroy; override;
    procedure InsertHeader;
    procedure InsertFooter;
    function ParamNameIndex(str: string): Integer; //...returns -1 if Name doesn't exist
    function ParamValueIndex(str: string): Integer; //...returns -1 if Name doesn't exist
  end;

implementation

uses
  JvTypes;

constructor TJvgCGI.Create(AutoHeaderAndFooter: Boolean);
//var
//  sMsg: string;
begin
  inherited Create;
  if not IsConsole then
    EJVCLException.Create('TJvgCGI not allowed in GUI application');

  FillChar(Val, SizeOf(Val), #0);

  Reset(Input);
  //_____________________________GET METHOD
  GetEnvironmentVariable('QUERY_STRING', @Val, SizeOf(Val));
  //_____________________________POST METHOD
//  if Val='' then
//  begin Read(Input,Val); sMsg := 'Post'; end
//  else sMsg := 'Get';

//  MessageBox(0,Val, PChar(sMsg+' method detected.'),MB_OK); //exit;

  FAutoHeaderAndFooter := AutoHeaderAndFooter;
  if FAutoHeaderAndFooter then
    InsertHeader;

  Val := ' ';
  //Val := 'order&p=5&u=55&c=98';
  //  Val := 'EDITION=Piter&?AUTHOR=&TITLE=%DF%E7%FB%EA+%EF%F0%EE%E3%F0%E0%EC%EC%E8%F0%EE%E2%E0%ED%E8%FF+%D4%EE%F0%F2&PB_ADD=%C4%EE%E1%E0%E2%E8%F2%FC';
  //  Names := TStringList.Create;
  //  Values := TStringList.Create;
  Params := TStringList.Create;
  if Length(string(Val)) > 0 then
  begin
    Split(Val);
    //    Decode( Values );
    Decode(Params);
  end;
end;

destructor TJvgCGI.Destroy;
begin
  if FAutoHeaderAndFooter then
    InsertFooter;
  //  Names.Free;
  //  Values.Free;
  Params.Free;
  inherited Destroy;
end;

procedure TJvgCGI.InsertHeader;
begin
  Writeln('HTTP/1.0 200 OK');
  Writeln('Date: Thursday, 12-Jan-96 16:04:30 GMT');
  Writeln('Server: WebSite 1.0');
  Writeln('Content-type: text/html');
  Writeln('Last-modified: Thursday, 25-Jan-96 16:04:30 GMT');
  Writeln('Content-length: 5000');
  Writeln('');
end;

procedure TJvgCGI.InsertFooter;
begin
  Writeln('Debug: ' + Val); //...debug
  Writeln('<META IDENTITY CONTENT="' + IntToStr(Random(100000)) + '">');
  Writeln('</BODY>');
  Writeln('</HTML>');
end;

procedure TJvgCGI.Split(S: string);
var
  I, LastI: Integer;

  procedure Extract(LastI, I: Integer);
//  var
//    J: Integer;
  begin
    {    J := LastI;
        while (S[J]<>'=')and(S[J]<>#0) do inc(J);
        Names.Add( Copy( S, LastI, J-LastI ) );
        Values.Add( Copy( S, J+1, I-J-1 ) );}
    Params.Add(Copy(S, LastI, I - LastI + 1));
  end;
begin
  I := 1;
  LastI := 1;
  while I < Length(S) do
  begin
    if (S[I] = '&') or (S[I] = '?') then
    begin
      Extract(LastI, I - 1);
      LastI := I + 1;
    end;
    Inc(I);
  end;
  Extract(LastI, I + 1);
end;

procedure TJvgCGI.Decode(sl: TStringList);
var
  I, J: Integer;
  S: string;
begin
  for I := 0 to sl.Count - 1 do
  begin
    S := sl[I];
    for J := 1 to Length(S) do
      if S[J] = '+' then
        S[J] := ' '
      else
      if S[J] = '%' then
        try
          S := Copy(S, 0, J - 1) + Char(StrToInt('$' + Copy(S, J + 1, 2))) + Copy(S, J + 3, Length(S) - J - 2);
        except
        end;
    if S[1] = '?' then
      S := Copy(S, 2, Length(S) - 1);
    sl[I] := S;

  end;
end;

function TJvgCGI.ParamNameIndex(str: string): Integer;
var
  I: Integer;
begin
  for I := 0 to Params.Count - 1 do
    if CompareText(Params.Names[I], str) = 0 then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function TJvgCGI.ParamValueIndex(str: string): Integer;
//var
//  I: Integer;
begin
  {  for I:=0 to Params.Count-1 do
      if CompareText( Params.Values[I], str ) = 0 then exit;
      begin Result := I; exit; end;}
  Result := -1;
end;

end.
