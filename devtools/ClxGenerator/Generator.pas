{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Generator.pas, released on 2004-02-12

The Initial Developer of the Original Code is Andreas Hausladen [Andreas.Hausladen@gmx.de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s):
  André Snepvangers [asn@xs4all.nl]

Last Modified: 2004-02-13

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
unit Generator;

interface

uses
  SysUtils, Classes, dpp_PascalParser, StrUtils;

type
  TGenerator = class(TObject)
  private
    FOutDir: string;
    FIndependentFiles: TStrings;

    function IsOnlyInclude(Token: PTokenInfo): Boolean;
//    function ParseUnit(Parser: TPascalParser): Boolean;
    procedure ParseUses(Parser: TPascalParser);
  protected
    function GetQName(const Filename: string): string;
    function GetIncFilename(const Filename: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    function GenerateQUnit(const Filename: string): Boolean;

    property OutDir: string read FOutDir write FOutDir;
  end;

function IsNotQFile(const Filename: string): Boolean;

implementation

function IsNotQFile(const Filename: string): Boolean;
var S: string;
begin

  S := ExtractFileName(Filename);
  Result := (S[3] <> 'Q') or
            ((S[3] = 'Q') and (S[4] <> 'Q')); // there are files like JvQuick...
end;

{ TGenerator }

constructor TGenerator.Create;
begin
  inherited Create;
  FIndependentFiles := TStringList.Create;
//  FIndependentFiles.Add('JVCLVer')
end;

destructor TGenerator.Destroy;
begin
  FIndependentFiles.Free;
  inherited;
end;

function TGenerator.GetQName(const Filename: string): string;
var Fn: string;
begin
  Result := ExtractFilePath(Filename);
  Fn := ExtractFileName(Filename);
  if AnsiStartsText('Jv', Fn) then
    Insert('Q', Fn, 3)
  else
    Fn := 'Q' + Fn;  
  Result := Result + Fn;
end;

function TGenerator.GetIncFilename(const Filename: string): string;
begin
  Result := ExtractRelativePath(FOutDir + PathDelim, ExtractFilePath(Filename)) + ExtractFileName(Filename);
end;

function TGenerator.GenerateQUnit(const Filename: string): Boolean;
var
  Lines: TStrings;
  Token: PTokenInfo;
  Parser: TPascalParser;
begin
  Result := False;
  if FIndependentFiles.IndexOf(ChangeFileExt(ExtractFileName(Filename), '')) <> -1 then
    Exit;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(Filename);

    Parser := TPascalParser.Create(ExtractFileName(Filename), Lines.Text);
    try
      while Parser.GetToken(Token) do
      begin
        if Token.Kind = tkComment then
          if IsOnlyInclude(Token) then
            Exit // do not generate a Q-unit
          else
            if Token.Value = '{$I JVCL.INC}' then
              Parser.Replace(Token, Token, '{$I jvcl.inc}');
        if Token.Kind = tkIdent then
        begin
(*)
        if CompareText(Token.Value, 'unit') = 0 then
          if not ParseUnit(Parser) then
        if not ParseUses(Parser) then
            Exit; // unit already processed
(*)
          if (CompareText(Token.Value, 'uses') = 0) or
             (CompareText(Token.Value, 'unit') = 0)
          then
            ParseUses(Parser); // add JvQxxxx lines for JvQxxxx units
        end;
      end;
//      if Parser.Modified then
      begin
        Lines.Text := Parser.Text;
        Lines.SaveToFile(FOutDir + PathDelim + GetQName(ExtractFileName(Filename)));
      end;
    finally
      Parser.Free;
    end;
    (*)
   // create Q-unit:
    Lines.Clear;
    Lines.Add(Format('unit %s;', [GetQName(ExtractFileName(Filename))]));
    Lines.Add('{$DEFINE QUnit}');
    Lines.Add(Format('{$INCLUDE ''%s''}', [GetIncFilename(Filename)]));
    Lines.SaveToFile(FOutDir + PathDelim + GetQName(ExtractFileName(Filename)));
    (*)
  finally
    Lines.Free;
  end;
  Result := True;
end;

function TGenerator.IsOnlyInclude(Token: PTokenInfo): Boolean;
var
  S: string;
  Num: Integer;
begin
  S := Token.Value;
 // remove comment brackets
  if S[1] = '{' then Num := 1 else Num := 2;
  Delete(S, 1, Num);
  Delete(S, Length(S) - Num + 1, Num);
  S := Trim(S);
  if AnsiStartsText('$INCLUDE ', S) or AnsiStartsText('$I ', S) then
    Result := (Pos('vclonly.inc', AnsiLowerCase(S)) > 0)
  else
    Result := False;
end;

procedure TGenerator.ParseUses(Parser: TPascalParser);
const
  CR = sLineBreak;
var
  Token: PTokenInfo;
  StartIndex: Integer;
//  NeedLeadingCR: Boolean;

  function MakeQUnits(const S: string): string;
  var
    Parser: TPascalParser;
    Token: PTokenInfo;
  begin
    Parser := TPascalParser.Create('', S);
    try
      while Parser.GetToken(Token) do
      begin
        if Token.Kind = tkIdent then
          Parser.ReplaceParseNext(Token, Token, GetQName(Token.Value));
      end;
      Result := Parser.Text;
    finally
      Parser.Free;
    end;
  end;

  procedure Replace;
  var
    S, NewS: string;
  begin
    S := Parser.GetPlainText(StartIndex, Token.StartIndex - 1);
    NewS := MakeQUnits(S);
(*)
   NewS := Format('{$IFDEF QUnit}'+CR+'  %s'+CR+'  {$ELSE}'+CR+
                      '  %s'+CR+'  {$ENDIF QUnit}'+CR+'  ',
             [MakeQUnits(S), S]);

    if NeedLeadingCR then
      NewS := CR + '  ' + NewS;
(*)
    Parser.Replace(StartIndex, Length(S), NewS);
    Parser.Index := StartIndex + Length(NewS);
    StartIndex := -1;
  end;

begin
  StartIndex := -1;
//  NeedLeadingCR := True;
  while Parser.GetToken(Token) do
  begin
    if Token.Kind = tkIdent then
    begin
      if AnsiStartsText('Jv', Token.Value) then
      begin
        if IsNotQFile(Token.Value) and
           (FIndependentFiles.IndexOf(Token.Value) = -1) then
        begin
          if StartIndex = -1 then
          begin
            StartIndex := Token.StartIndex;
//            NeedLeadingCR := Parser.PreToken.EndLine = Token.StartLine;
          end;
        end
        else if StartIndex <> -1 then
          Replace;
      end
      else                  // asn: added
        StartIndex := -1;
    end
    else if (Token.Kind = tkSymbol) and (Token.Value = ';') then
    begin
      if StartIndex <> -1 then
        Replace;
      Break;
    end;
  end;
end;

(*)
function TGenerator.ParseUnit(Parser: TPascalParser): Boolean;
var
  Token: PTokenInfo;
  DoInsert: Boolean;
  StartIndex: Integer;
  S: string;
  Num: Integer;
begin
  DoInsert := True;
  Token := Parser.PreToken;
  if (Token <> nil) and (Token.Kind = tkComment) then
  begin
    S := Token.Value;
   // remove comment brackets
    if S[1] = '{' then Num := 1 else Num := 2;
    Delete(S, 1, Num);
    Delete(S, Length(S) - Num + 1, Num);
    DoInsert := AnsiCompareText('$IFDEF QUnit', S) <> 0;
  end;

  if DoInsert then
  begin
    StartIndex := Parser.CurToken.StartIndex;
    while Parser.GetToken(Token) do
      if Token.Value = ';' then
        Break;
    if Token <> nil then
    begin
      Parser.Insert(Parser.CurToken.EndIndex + 1, sLineBreak + '{$ENDIF QUnit}');
      Parser.Insert(StartIndex, '{$IFDEF QUnit}' + sLineBreak);
      Parser.Index := StartIndex;
      Parser.GetToken; // IFDEF
      Parser.GetToken; // unit
    end
    else
      DoInsert := False;
  end;
  Result := DoInsert;
end;
(*)

end.
