{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHtmlParser.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].
                Alexander Samusenko[sandx@chat.ru].

Last Modified: 2001-09-20

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvHtmlParser;



interface

uses
  Windows, SysUtils, Classes, JvTypes, JclStrings, JvComponent;

type
  PTagInfo = ^TTagInfo;

  TTagInfo = record
    BeginPos: integer;
    EndPos: integer;
    BeginContext: integer;
    EndContext: integer;
    Key: Integer;
  end;

  TTagInfoList = class(TList)
  public
    procedure AddValue(Value: TTagInfo);
    procedure Clear; override;
  end;

  TJvHtmlParser = class(TJvComponent)
  private
    FOnKeyFound: TOnKeyFound;
    FInfos: TParserInfos;
    FKeys: TStringList;
    FFile: TFileName;
    FTagList: TTagInfoList;
    procedure SetInfos(Value: TParserInfos);
    procedure SetFile(Value: TFileName);
    procedure SetTagList(const Value: TTagInfoList);
    function GetConditionsCount: integer;
  protected
    procedure Loaded; override;
    property TagList: TTagInfoList read FTagList write SetTagList;
  public
    procedure AnalyseString(Str: string);
    procedure AnalyseFile;
    procedure AddCondition(Keyword, StartTag, EndTag: string; TextSelection:
      integer = 0);
    procedure RemoveCondition(index: Integer);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ConditionsCount: integer read GetConditionsCount;
    procedure ClearConditions;
    procedure GetCondition(Index: Integer; var Keyword: string; var StartTag:
      string; var EndTag: string); overload;
    procedure GetCondition(Index: Integer; var Keyword: string; var StartTag:
      string; var EndTag: string; var TextSelection: integer); overload;

  published
    property OnKeyFound: TOnKeyFound read FOnKeyFound write FOnKeyFound;
    property FileName: TFileName read Ffile write SetFile;
    property Parser: TParserInfos read FInfos write SetInfos;
  end;

  {Comparison function. Used internally  for  observance of the sequences tags}
function CompareTags(Item1, Item2: Pointer): Integer;

implementation
{*************************************************}

function CompareTags(Item1, Item2: Pointer): Integer;
begin
  Result := (PTagInfo(Item1).BeginPos - PTagInfo(Item2).BeginPos);
end;

{*************************************************}

constructor TJvHtmlParser.Create(AOwner: TComponent);
begin
  inherited;
  Finfos := TStringList.Create;
  FKeys := TStringList.Create;
  FTagList := TTagInfoList.Create;
end;

{*************************************************}

destructor TJvHtmlParser.Destroy;
begin
  FInfos.Free;
  FKeys.Free;
  FTagList.Free;
  inherited;
end;

{*************************************************}

procedure TJvHtmlParser.SetFile(Value: TFileName);
begin
  if FFile <> Value then
  begin
    FFile := Value;
    if not (csDesigning in Self.ComponentState) then
      AnalyseFile;
  end;
end;

{*************************************************}

procedure TJvHtmlParser.SetInfos(Value: TParserInfos);
var
  i: Integer;
  ob: TParserInf;
  cap: string;
begin
  FInfos := Value;
  FKeys.Clear;
  i := 0;
  while i < Finfos.Count do
  begin
    ob := TParserInf.Create;
    try
      cap := FInfos[i];
      Inc(i);
      ob.StartTag := FInfos[i];
      Inc(i);
      ob.EndTag := FInfos[i];
      Inc(i);
      ob.MustBe := StrToInt(FInfos[i]);
      Inc(i);
      ob.TakeText := StrToInt(FInfos[i]);
      Inc(i);
    finally
      FKeys.AddObject(cap, TObject(ob));
    end;
  end;
end;

{*************************************************}

procedure TJvHtmlParser.AnalyseFile;
var
  fich: TextFile;
  buf, st: string;
begin
  st := '';
  try
    AssignFile(fich, FileName);
    Reset(fich);
    buf := '';
    while not Eof(fich) do
    begin
      Readln(fich, buf);
      st := st + buf;
    end;
  finally
    CloseFile(Fich);
  end;
  AnalyseString(st);
end;

{*************************************************}

procedure TJvHtmlParser.AnalyseString(Str: string);
var
  st2: string;
  i, j, k, index: Integer;
  TagInfo: TTagInfo;
begin
  if (FKeys.Count = 0) and (FInfos.Count <> 0) then
    SetInfos(FInfos);
  if FKeys.Count > 0 then
  begin
    FTagList.Clear;
    for i := 0 to FKeys.Count - 1 do
    begin
      j := 1;
      while (j <> 0) do
      begin
        j := StrSearch(TParserInf(FKeys.Objects[i]).StartTag, Str, j);
        if j > 0 then
        begin
          k := StrSearch(TParserInf(FKeys.Objects[i]).EndTag, Str, j);
          TagInfo.BeginPos := j;
          TagInfo.EndPos := k + length(TParserInf(FKeys.Objects[i]).EndTag);
          TagInfo.Key := i;
          case TParserInf(FKeys.Objects[i]).TakeText of
            0: //Between limits
              begin
                TagInfo.BeginContext := j +
                  Length(TParserInf(FKeys.Objects[i]).StartTag);
                TagInfo.EndContext := k;
              end;
            1: //All before start tag
              begin
                TagInfo.BeginContext := 1;
                TagInfo.EndContext := j;
              end;
            2: //All after start tag
              begin
                TagInfo.BeginContext := j +
                  Length(TParserInf(FKeys.Objects[i]).StartTag);
                TagInfo.EndContext := Length(Str);
              end;
            3: //The whole line if containing start tag
              begin
                TagInfo.BeginContext := j;
                TagInfo.EndContext := StrSearch('#1310', Str, j);
              end;
          end;
          FTagList.AddValue(TagInfo);
          j := j + Length(TParserInf(FKeys.Objects[i]).StartTag);
        end;
      end;
    end;
  end;
  FTagList.Sort(@CompareTags);
  with FTagList do
  begin
    for index := 0 to (Count - 1) do
    begin
      st2 := Copy(Str, PTagInfo(Items[index]).BeginContext,
        PTagInfo(Items[index]).EndContext -
          PTagInfo(Items[index]).BeginContext);
      if Assigned(FOnKeyFound) then
        FOnKeyFound(Self, FKeys[PTagInfo(Items[index]).Key], st2, Str);
    end;
  end;
end;
{*************************************************}

procedure TJvHtmlParser.AddCondition(Keyword, StartTag, EndTag: string;
  TextSelection: integer = 0);
var
  ob: TParserInf;
begin
  ob := TParserInf.Create;
  ob.StartTag := StartTag;
  ob.EndTag := EndTag;
  ob.TakeText := TextSelection;
  FKeys.AddObject(Keyword, TObject(ob));
end;

{*************************************************}

procedure TJvHtmlParser.RemoveCondition(index: Integer);
begin
  FKeys.Delete(index);
end;

{*************************************************}

procedure TJvHtmlParser.ClearConditions;
begin
  FInfos.Clear;
  FKeys.Clear;
end;

{*************************************************}

procedure TJvHtmlParser.GetCondition(Index: Integer; var Keyword: string; var
  StartTag: string; var EndTag: string);
begin
  Keyword := FKeys[Index];
  StartTag := TParserInf(FKeys.Objects[Index]).StartTag;
  EndTag := TParserInf(FKeys.Objects[Index]).EndTag;
end;
{*************************************************}
procedure TJvHtmlParser.GetCondition(Index: Integer; var Keyword, StartTag,
  EndTag: string; var TextSelection: integer);
begin
  Self.GetCondition(Index, Keyword, StartTag, EndTag);
  TextSelection := TParserInf(FKeys.Objects[Index]).TakeText;
end;
{*************************************************}

function TJvHtmlParser.GetConditionsCount: integer;
begin
  Result := FKeys.Count;
end;
{*************************************************}

procedure TJvHtmlParser.SetTagList(const Value: TTagInfoList);
begin
  FTagList := Value;
end;
{*************************************************}

procedure TJvHtmlParser.Loaded;
begin
  inherited;
  SetInfos(FInfos);
end;

{ TTagInfoList }

{*************************************************}

procedure TTagInfoList.AddValue(Value: TTagInfo);
var
  P: PTagInfo;
begin
  GetMem(P, SizeOf(TTagInfo));
  if P <> nil then
  begin
    with P^ do
    begin
      BeginPos := Value.BeginPos;
      EndPos := Value.EndPos;
      BeginContext := Value.BeginContext;
      EndContext := Value.EndContext;
      Key := Value.Key;
    end;
    Self.Add(P);
  end;
end;
{*************************************************}

procedure TTagInfoList.Clear;
var
  i: integer;
begin
  for i := 0 to Self.Count - 1 do
  begin
    FreeMem(Self.Items[i], SizeOf(TTagInfo));
  end;
  inherited;
end;

end.

