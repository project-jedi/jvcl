{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHTMLParser.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].
                Alexander Samusenko[sandx att chat dott ru].
                CarlEfird.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvHtmlParser;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  JclStrings,
  JvComponentBase;

type
  PTagInfo = ^TTagInfo;

  TTagInfo = record
    BeginPos: Integer;
    EndPos: Integer;
    BeginContext: Integer;
    EndContext: Integer;
    Key: Integer;
  end;

  // (rom) definitely needs improvement
  TJvParserInfo = class(TObject)
  public
    StartTag: string;
    EndTag: string;
    MustBe: Integer;
    TakeText: Integer;
  end;

  TTagInfoList = class(TList)
  public
    procedure AddValue(const Value: TTagInfo);
    procedure Clear; override;
  end;

  TJvKeyFoundEvent = procedure(Sender: TObject; Key, Results, OriginalLine: string) of object;
  TJvKeyFoundExEvent = procedure(Sender: TObject; Key, Results, OriginalLine: string;
    TagInfo: TTagInfo; Attributes: TStrings) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF RTL230_UP}
  TJvHTMLParser = class(TJvComponent)
  private
    FParser: TStringList;
    FKeys: TStringList;
    FFileName: TFileName;
    FTagList: TTagInfoList;
    FContent: string;
    FOnKeyFound: TJvKeyFoundEvent;
    FOnKeyFoundEx: TJvKeyFoundExEvent;
    function GetParser: TStrings;
    procedure SetParser(Value: TStrings);
    procedure SetFileName(Value: TFileName);
    procedure SetTagList(const Value: TTagInfoList);
    function GetConditionsCount: Integer;
  protected
    procedure Loaded; override;
    property TagList: TTagInfoList read FTagList write SetTagList;
  public
    procedure AnalyseString(const Str: string);
    procedure AnalyseFile;
    procedure AddCondition(const Keyword: string; const StartTag: string = '<';
      const EndTag: string = '>'; TextSelection: Integer = 0);
    procedure RemoveCondition(Index: Integer);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ConditionsCount: Integer read GetConditionsCount;
    procedure ClearConditions;
    procedure GetCondition(Index: Integer; var Keyword, StartTag, EndTag: string); overload;
    procedure GetCondition(Index: Integer; var Keyword, StartTag, EndTag: string;
      var TextSelection: Integer); overload;
    property Content: string read FContent;
  published
    property FileName: TFileName read FFileName write SetFileName;
    property Parser: TStrings read GetParser write SetParser;
    property OnKeyFound: TJvKeyFoundEvent read FOnKeyFound write FOnKeyFound;
    property OnKeyFoundEx: TJvKeyFoundExEvent read FOnKeyFoundEx write FOnKeyFoundEx;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFNDEF COMPILER12_UP}
  JvJCLUtils,
  {$ENDIF ~COMPILER12_UP}
  JvConsts;

{Comparison function. Used internally for observance of the sequences tags}
function CompareTags(Item1, Item2: Pointer): Integer;
begin
  Result := (PTagInfo(Item1).BeginPos - PTagInfo(Item2).BeginPos);
end;

//=== { TJvHTMLParser } ======================================================

constructor TJvHTMLParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParser := TStringList.Create;
  FKeys := TStringList.Create;
  FTagList := TTagInfoList.Create;
end;

destructor TJvHTMLParser.Destroy;
var
  Index: Integer;
begin
  FParser.Free;
  for Index := FKeys.Count - 1 downto 0 do
    FKeys.Objects[Index].Free;
  FKeys.Free;
  FTagList.Free;
  inherited Destroy;
end;

procedure TJvHTMLParser.Loaded;
begin
  inherited Loaded;
  SetParser(FParser);
end;

procedure TJvHTMLParser.SetFileName(Value: TFileName);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
    if not (csDesigning in ComponentState) then
      AnalyseFile;
  end;
end;

function TJvHTMLParser.GetParser: TStrings;
begin
  Result := FParser;
end;

procedure TJvHTMLParser.SetParser(Value: TStrings);
var
  I: Integer;
  Obj: TJvParserInfo;
  Cap: string;
begin
  if FParser <> Value then // make sure we don't assign to ourselves (that will clear the list)
    FParser.Assign(Value);
  for I := FKeys.Count - 1 downto 0 do
    FKeys.Objects[I].Free;
  FKeys.Clear;
  I := 0;
  while I < FParser.Count do
  begin
    Obj := TJvParserInfo.Create;
    try
      Cap := FParser[I];
      Inc(I);
      Obj.StartTag := FParser[I];
      Inc(I);
      Obj.EndTag := FParser[I];
      Inc(I);
      Obj.MustBe := StrToInt(FParser[I]);
      Inc(I);
      Obj.TakeText := StrToInt(FParser[I]);
      Inc(I);
    finally
      FKeys.AddObject(Cap, Obj);
    end;
  end;
end;

// (rom) reimplemented with a TStringList

procedure TJvHTMLParser.AnalyseFile;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    if FileExists(FileName) then
    begin
      List.LoadFromFile(FileName);
      AnalyseString(List.Text);
    end;
  finally
    List.Free;
  end;
end;

procedure TJvHTMLParser.AnalyseString(const Str: string);
var
  Str2, Str3: string;
  StartTag1, StartTag2: string;
  I, J, J1, K, Index: Integer;
  TagInfo: TTagInfo;
  AttributesList: TStringList;

  procedure InnerParseAttributes(Content: PChar;  Strings: TStrings);
  var
    Head, Tail: PChar;
    EOS, InQuote, LeadQuote: Boolean;
    QuoteChar: Char;
  begin
    if (Content = nil) or (Content^ = #0) then
      Exit;
    Tail := Content;
    QuoteChar := #0;
    repeat
      while CharInSet(Tail^, [Cr, Lf, ' ']) do
        Inc(Tail);
      Head := Tail;
      InQuote := False;
      LeadQuote := False;
      while True do
      begin
        while (InQuote and not CharInSet(Tail^, [#0, '"'])) or
              not CharInSet(Tail^, [#0, Cr, Lf, ' ', '"']) do
          Inc(Tail);
        if Tail^ = '"' then
        begin
          if (QuoteChar <> #0) and (QuoteChar = Tail^) then
            QuoteChar := #0
          else
          begin
            LeadQuote := Head = Tail;
            QuoteChar := Tail^;
            if LeadQuote then
              Inc(Head);
          end;
          InQuote := QuoteChar <> #0;
          if InQuote then
            Inc(Tail)
          else
            Break;
        end
        else
          Break;
      end;
      if not LeadQuote and (Tail^ <> #0) and (Tail^ = '"') then
        Inc(Tail);
      EOS := Tail^ = #0;
      Tail^ := #0;
      if Head^ <> #0 then
        Strings.Add(Head);
      Inc(Tail);
    until EOS;
  end;

  procedure ParseAttributes(Strings: TStrings; const Value: string);
  var
    P: PChar;
    Tmp: string;
  begin
    Strings.Clear;
    Tmp := Value;
    UniqueString(Tmp);
    P := PChar(Tmp);
//    if P^ in [#0, '<', '>'] then
    if CharInSet(P^, [#0, '>']) then
      Exit;
    // skip first word (the tag) and any whitespace
    while (P^ <> #0) and (P <> nil) do
    begin
      if P^ = ' ' then
      begin
        Inc(P);
        Break;
      end;
      Inc(P);
    end;
    InnerParseAttributes(P, Strings);
  end;

begin
  if (FKeys.Count = 0) and (FParser.Count <> 0) then
    SetParser(FParser);
  FContent := Str;
  AttributesList := TStringList.Create;
  try
    if FKeys.Count > 0 then
    begin
      FTagList.Clear;
      for I := 0 to FKeys.Count - 1 do
      begin
        StartTag1 := TJvParserInfo(FKeys.Objects[I]).StartTag;
        Starttag2 := '';
        if (Length(StartTag1) > 2) and (StartTag1[Length(StartTag1)] = '>') then
        begin
          // split the tag so tags with attributes can be found
          Delete(StartTag1, Length(StartTag1), 1);
          StartTag2 := '>';
        end;
        J := 1;
        while J <> 0 do
        begin
          // changed from StrSearch(case sensitive) to StrFind (case insensitive)
          if trim(StartTag2) = '' then
          begin
            J := StrFind(StartTag1, Str, J)
          end
          else
          begin
            J1 := J;
            J := StrFind(StartTag1 + ' ', Str, J);
            if J = 0 then
             J := StrFind(StartTag1 + '>', Str, J1);
          end;
          if J > 0 then
          begin
            // changed from StrSearch(case sensitive) to StrFind (case insensitive)
            K := StrFind(TJvParserInfo(FKeys.Objects[I]).EndTag, Str, J);
            TagInfo.BeginPos := J;
            TagInfo.EndPos := K + Length(TJvParserInfo(FKeys.Objects[I]).EndTag);
            TagInfo.Key := I;
            case TJvParserInfo(FKeys.Objects[I]).TakeText of
              0: //Between limits
                begin
                  if StartTag2 = '' then
                    TagInfo.BeginContext := J + Length(TJvParserInfo(FKeys.Objects[I]).StartTag)
                  else
                    // changed from StrSearch(case sensitive) to StrFind (case insensitive)
                    TagInfo.BeginContext := StrFind(StartTag2, Str, j) + 1;
                  TagInfo.EndContext := K;
                end;
              1: //All before start tag
                begin
                  TagInfo.BeginContext := 1;
                  TagInfo.EndContext := J;
                end;
              2: //All after start tag
                begin
                  if StartTag2 = '' then
                    TagInfo.BeginContext := J + Length(TJvParserInfo(FKeys.Objects[I]).StartTag)
                  else
                    // changed from StrSearch(case sensitive) to StrFind (case insensitive)
                    TagInfo.BeginContext := StrFind(StartTag2, Str, j) + 1;
                  TagInfo.EndContext := Length(Str);
                end;
              3: //The whole line if containing start tag
                begin
                  TagInfo.BeginContext := J;
                  // changed from StrSearch(case sensitive) to StrFind (case insensitive)
                  TagInfo.EndContext := StrFind(Lf, Str, J);
                end;
            end;
            FTagList.AddValue(TagInfo);
            J := J + Length(TJvParserInfo(FKeys.Objects[I]).StartTag);
          end;
        end;
      end;
    end;
    FTagList.Sort(CompareTags);
    with FTagList do
    begin
      for Index := 0 to Count - 1 do
      begin
        // Str2 now contains eveything between the start and end tags
        Str2 := Copy(Str, PTagInfo(Items[Index]).BeginContext,
          PTagInfo(Items[Index]).EndContext - PTagInfo(Items[Index]).BeginContext);
        if StartTag2 = '' then
          Str3 := ''
        else
          //Str3 contains the start tag as found, may include attributes or other tags
          Str3 := Copy(Str, PTagInfo(Items[Index]).BeginPos,
            PTagInfo(Items[Index]).BeginContext - PTagInfo(Items[Index]).BeginPos - 1);
        if Assigned(FOnKeyFound) then
          FOnKeyFound(Self, FKeys[PTagInfo(Items[Index]).Key], Str2, Str);
        if Assigned(FOnKeyFoundEx) then
        begin
          if Str3 <> '' then
            ParseAttributes(AttributesList, Str3)
          else
            ParseAttributes(AttributesList, Str2);
          FOnKeyFoundEx(Self, FKeys[PTagInfo(Items[Index]).Key], Str2, Str,
            PTagInfo(Items[Index])^, AttributesList);
        end;
      end;
    end;
  finally
    AttributesList.Free;
  end;
end;

procedure TJvHTMLParser.AddCondition(const Keyword: string;
  const StartTag: string; const EndTag: string; TextSelection: Integer);
var
  Obj: TJvParserInfo;
begin
  Obj := TJvParserInfo.Create;
  Obj.StartTag := StartTag;
  Obj.EndTag := EndTag;
  Obj.TakeText := TextSelection;
  FKeys.AddObject(Keyword, TObject(Obj));
end;

procedure TJvHTMLParser.RemoveCondition(Index: Integer);
begin
  FKeys.Objects[Index].Free;
  FKeys.Delete(Index);
end;

procedure TJvHTMLParser.ClearConditions;
var
  Index: Integer;
begin
  FParser.Clear;
  for Index := FKeys.Count - 1 downto 0 do
    FKeys.Objects[Index].Free;
  FKeys.Clear;
end;

procedure TJvHTMLParser.GetCondition(Index: Integer; var Keyword, StartTag, EndTag: string);
begin
  Keyword := FKeys[Index];
  StartTag := TJvParserInfo(FKeys.Objects[Index]).StartTag;
  EndTag := TJvParserInfo(FKeys.Objects[Index]).EndTag;
end;

procedure TJvHTMLParser.GetCondition(Index: Integer; var Keyword, StartTag, EndTag: string;
  var TextSelection: Integer);
begin
  GetCondition(Index, Keyword, StartTag, EndTag);
  TextSelection := TJvParserInfo(FKeys.Objects[Index]).TakeText;
end;

function TJvHTMLParser.GetConditionsCount: Integer;
begin
  Result := FKeys.Count;
end;

procedure TJvHTMLParser.SetTagList(const Value: TTagInfoList);
begin
  FTagList := Value;
end;

//=== { TTagInfoList } =======================================================

procedure TTagInfoList.AddValue(const Value: TTagInfo);
var
  P: PTagInfo;
begin
  GetMem(P, SizeOf(TTagInfo));
  if P <> nil then
  begin
    P^ := Value;
    Add(P);
  end;
end;

procedure TTagInfoList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FreeMem(Items[I], SizeOf(TTagInfo));
  inherited Clear;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
