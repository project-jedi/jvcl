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

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvHtmlParser;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvTypes, JvComponent;

type
  TJvHtmlParser = class(TJvComponent)
  private
    FOnKeyFound: TOnKeyFound;
    FInfos: TParserInfos;
    FKeys: TStringList;
    FFile: TFileName;
    procedure SetInfos(Value: TParserInfos);
    procedure SetFile(Value: TFileName);
    function FirstLine(var Value: string): string;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    procedure AnalyseFile;
    procedure AddCondition(Keyword, StartTag, EndTag: string);
    procedure RemoveCondition(index: Integer);
    procedure ClearConditions;
    procedure GetCondition(Index: Integer; var Keyword: string; var StartTag: string; var EndTag: string);
    property OnKeyFound: TOnKeyFound read FOnKeyFound write FOnKeyFound;
    property FileName: TFileName read Ffile write SetFile;
    property Parser: TParserInfos read FInfos write SetInfos;
  end;

implementation

{*************************************************}

constructor TJvHtmlParser.Create(AOwner: TComponent);
begin
  inherited;
  Finfos := TStringList.Create;
  FKeys := TStringList.Create;
  SetInfos(Finfos);
end;

{*************************************************}

destructor TJvHtmlParser.Destroy;
begin
  FInfos.Free;
  FKeys.Free;
  inherited;
end;

{*************************************************}

procedure TJvHtmlParser.SetFile(Value: TFileName);
begin
  FFile := Value;
  AnalyseFile;
end;

{*************************************************}

function TJvHtmlParser.FirstLine(var Value: string): string;
begin
  if Pos(#10, Value) = 0 then
  begin
    Result := Value;
    Value := '';
  end
  else
  begin
    Result := Copy(Value, 1, Pos(#10, Value) - 1);
    Value := Copy(Value, Pos(#10, Value) + 1, Length(Value));
    if Pos(#13, Value) = 1 then
      Value := Copy(Value, 2, Length(Value));
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
  buf, st, st2: string;
  i, j: Integer;
begin
  if (FKeys.Count = 0) and (FInfos.Count <> 0) then
    SetInfos(FInfos);
  if FKeys.Count > 0 then
  begin
    try
      AssignFile(fich, FileName);
      Reset(fich);
      buf := '';
      while not Eof(fich) or (buf <> '') do
      begin
        if buf = '' then
        begin
          Readln(fich, buf);
          st := FirstLine(buf);
        end
        else
          st := FirstLine(buf);

        for i := 0 to FKeys.Count - 1 do
        begin
          j := Pos(TParserInf(FKeys.Objects[i]).StartTag, st);
          if (j = TParserInf(FKeys.Objects[i]).MustBe) or
            ((j <> 0) and (TParserInf(FKeys.Objects[i]).MustBe = -1)) then
          begin
            case TParserInf(FKeys.Objects[i]).TakeText of
              0:
                begin
                  st2 := Copy(st, j + Length(TParserInf(FKeys.Objects[i]).StartTag), Length(st));
                  j := Pos(TParserInf(FKeys.Objects[i]).EndTag, st2);
                  if j <> 0 then
                    st2 := Copy(st2, 1, j - 1);
                end;
              1: st2 := Copy(st, 1, j - 1);
              2: st2 := Copy(st, j + Length(TParserInf(FKeys.Objects[i]).StartTag), Length(st));
              3: st2 := st;
            end;
            //0 Between limits
            //1 All before start tag
            //2 All after start tag
            //3 The whole line if containing start tag
            if Assigned(FOnKeyFound) then
              FOnKeyFound(Self, FKeys[i], st2, st);
          end;
        end;
      end;
    finally
      CloseFile(Fich);
    end;
  end;
end;

{*************************************************}

procedure TJvHtmlParser.AddCondition(Keyword, StartTag, EndTag: string);
var
  ob: TParserInf;
begin
  ob := TParserInf.Create;
  ob.StartTag := StartTag;
  ob.EndTag := EndTag;
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

procedure TJvHtmlParser.GetCondition(Index: Integer; var Keyword: string; var StartTag: string; var EndTag: string);
begin
  Keyword := FKeys[Index];
  StartTag := TParserInf(FKeys.Objects[Index]).StartTag;
  EndTag := TParserInf(FKeys.Objects[Index]).EndTag;
end;

end.
