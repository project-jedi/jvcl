{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCLConfiguration.pas, released on 2004-03-29.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JVCLConfiguration;

interface

uses
  SysUtils, Classes, Contnrs, dpp_PascalParser;

type
  TJVCLConfig = class;
  TJVCLConfigItem = class;

  TJVCLConfigItem = class(TObject)
  private
    FOwner: TJVCLConfig;
    FComment: string;
    FDirective: string;
    FLine: Integer;
    function GetEnabled: Boolean;
    function GetName: string;
    procedure SetEnabled(const Value: Boolean);
  public
    constructor Create(AOwner: TJVCLConfig; const AComment, ADirective: string;
      ALine: Integer);
    property Line: Integer read FLine;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Name: string read GetName;
    property Comment: string read FComment;
  end;

  // Do not edit the lines directly. If you must then call Parse after you had
  // done your changes.
  TJVCLConfig = class(TStringList)
  private
    FModified: Boolean;
    FItems: TObjectList;
    FFilename: string;
    procedure ParseComment(Token: PTokenInfo; var LastCommentToken: string;
      var InAutoConfig: Integer);
    function GetItemCount: Integer;
    function GetItems(Index: Integer): TJVCLConfigItem;
    function GetEnabled(Index: string): Boolean;
    procedure SetEnabled(Index: string; const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure Parse;
    procedure LoadFromFile(const AFilename: string); override;
    procedure LoadFromStream(Stream: TStream); override;

    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TJVCLConfigItem read GetItems;
    property Enabled[Index: string]: Boolean read GetEnabled write SetEnabled;
    property Modified: Boolean read FModified write FModified;
    property Filename: string read FFilename;
  end;

implementation

function RemoveCommentBrackets(const Comment: string): string;
var
  Len: Integer;
begin
  Result := Comment;
  if Result = '' then
    Exit;
  // remove comment brackets
  if Result[1] = '{' then
    Len := 1
  else
    Len := 2;
  Delete(Result, 1, Len);
  Delete(Result, Length(Result) - Len + 1, Len);
end;

function IsDirective(S: string; const Directive: string): Boolean;
begin
  Result := False;
  S := Trim(S);
  if StrLIComp(PChar(S), PChar(Directive), Length(Directive)) = 0 then
    Result := S[Length(Directive) + 1] in [#0..#32, '}', '*'];
end;


//=== TJVCLConfigItem ========================================================

constructor TJVCLConfigItem.Create(AOwner: TJVCLConfig;
  const AComment, ADirective: string; ALine: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FLine := ALine;
  FComment := AComment;
  FDirective := ADirective;
end;

function TJVCLConfigItem.GetEnabled: Boolean;
begin
  Result := FDirective[1] = '$';
end;

function TJVCLConfigItem.GetName: string;
var
  F, P: PChar;
begin
  P := PChar(FDirective);
  while (P[0] <> #0) and (P[0] > #32) do
    Inc(P);
  while (P[0] <> #0) and (P[0] <= #32) do
    Inc(P);
  F := P;
  while (P[0] <> #0) and (P[0] > #32) do
    Inc(P);
  SetString(Result, F, P - F);
end;

procedure TJVCLConfigItem.SetEnabled(const Value: Boolean);
begin
  if Value <> GetEnabled then
  begin
    if Value then
      Delete(FDirective, 1, Pos('$', FDirective) - 1)
    else
      Insert('.', FDirective, 1);
    FOwner[FLine] := '{' + FDirective + '}';
    FOwner.Modified := True;
  end;
end;

//=== TJVCLConfig ============================================================

constructor TJVCLConfig.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TJVCLConfig.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJVCLConfig.GetEnabled(Index: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    if CompareText(Index, Items[I].Name) = 0 then
    begin
      Result := Items[I].Enabled;
      Exit;
    end;
  Result := False;
end;

procedure TJVCLConfig.SetEnabled(Index: string; const Value: Boolean);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if CompareText(Index, Items[I].Name) = 0 then
    begin
      Items[I].Enabled := Value;
      Exit;
    end;
end;

function TJVCLConfig.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TJVCLConfig.GetItems(Index: Integer): TJVCLConfigItem;
begin
  Result := TJVCLConfigItem(FItems[Index]);
end;

procedure TJVCLConfig.LoadFromStream(Stream: TStream);
begin
  FFilename := '';
  inherited LoadFromStream(Stream);
  Parse;
end;

procedure TJVCLConfig.Parse;
var
  Parser: TPascalParser;
  LastCommentToken: string;
  InAutoConfig: Integer;
  Token: PTokenInfo;
begin
  FItems.Clear;

  Parser := TPascalParser.Create('', Text);
  try
    LastCommentToken := '';
    InAutoConfig := 0;
    while Parser.GetToken(Token) do
    begin
      if Token.Kind = tkComment then
        ParseComment(Token, LastCommentToken, InAutoConfig);
    end;
  finally
    Parser.Free;
  end;
  FModified := False;
end;

procedure TJVCLConfig.ParseComment(Token: PTokenInfo;
  var LastCommentToken: string; var InAutoConfig: Integer);
var
  S: String;
  Item: TJVCLConfigItem;
begin
  S := Trim(RemoveCommentBrackets(Token.Value));

  if IsDirective(S, '$IFDEF') or IsDirective(S, '$IF') or       // do not localize
    IsDirective(S, '$IFNDEF') then                              // do not localize
    Inc(InAutoConfig)
  else
  if IsDirective(S, '$ENDIF') or IsDirective(S, '$IFEND') then  // do not localize
    Dec(InAutoConfig);

  if InAutoConfig = 0 then
    if IsDirective(S, '$DEFINE') or IsDirective(S, '.$DEFINE') then // do not localize
    begin
      Item := TJVCLConfigItem.Create(Self, LastCommentToken, S, Token.StartLine - 1);
      FItems.Add(Item);
    end
    else
      LastCommentToken := S;
end;

procedure TJVCLConfig.LoadFromFile(const AFilename: string);
begin
  inherited LoadFromFile(AFilename);
  FFilename := AFilename;
end;

procedure TJVCLConfig.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Parse;
end;

end.
