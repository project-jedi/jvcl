{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgCGI.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

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
    FAutoHeaderAndFooter: boolean;
    procedure Split(s: string);
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
//  begin Read(Input,Val); sMsg := 'Post'; end else sMsg := 'Get';

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

procedure TJvgCGI.Split(s: string);
var
  i, lastI: Integer;

  procedure Extract(LastI, i: Integer);
//  var
//    j: Integer;
  begin
    {    j := LastI;
        while (s[j]<>'=')and(s[j]<>#0) do inc(j);
        Names.Add( Copy( s, LastI, j-LastI ) );
        Values.Add( Copy( s, j+1, i-j-1 ) );}
    Params.Add(Copy(s, LastI, i - LastI + 1));
  end;
begin
  i := 1;
  lastI := 1;
  while i < Length(s) do
  begin
    if (s[i] = '&') or (s[i] = '?') then
    begin
      Extract(LastI, i - 1);
      lastI := i + 1;
    end;
    Inc(i);
  end;
  Extract(LastI, i + 1);
end;

procedure TJvgCGI.Decode(sl: TStringList);
var
  i, j: Integer;
  s: string;
begin
  for i := 0 to sl.Count - 1 do
  begin
    s := sl[i];
    for j := 1 to Length(s) do
      if s[j] = '+' then
        s[j] := ' '
      else
      if s[j] = '%' then
      try
        s := Copy(s, 0, j - 1) + Char(StrToInt('$' + Copy(s, j + 1, 2))) + Copy(s, j + 3, Length(s) - j - 2);
      except
      end;
    if s[1] = '?' then
      s := Copy(s, 2, Length(s) - 1);
    sl[i] := s;

  end;
end;

function TJvgCGI.ParamNameIndex(str: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Params.Count - 1 do
    if CompareText(Params.Names[i], str) = 0 then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

function TJvgCGI.ParamValueIndex(str: string): Integer;
//var
//  i: Integer;
begin
  {  for i:=0 to Params.Count-1 do
      if CompareText( Params.Values[i], str ) = 0 then exit;
      begin Result := i; exit; end;}
  Result := -1;
end;

end.
