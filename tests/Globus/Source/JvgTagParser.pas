{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgTagParser.PAS, released on 2003-01-15.

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

unit JvgTagParser;

interface
uses Classes, SysUtils, httpapp;

type

  TJvgTagParser = class
  private
    TagParams: TStrings;
  public
    AttributeFilter: TStrings;
    constructor Create;
    destructor Destroy; override;
    function Attributes(const sTag: string): TStrings;
    procedure OnHTMLTag(Sender: TObject; Tag: TTag; const TagString: String; TagParams: TStrings; var ReplaceText: String);
  end;

implementation
//uses ;

function TJvgTagParser.Attributes(const sTag: string): TStrings;
var
  i: integer;
  PageProducer: TPageProducer;
  sTemp, sIncludeParamName, sIncludeParamValue: string;
begin
  Result := TStringList.Create;
  try
  PageProducer := TPageProducer.Create(nil);
  PageProducer.HTMLDoc.Text := StringReplace(sTag, '<?', '<#', []);
  PageProducer.OnHTMLTag := OnHTMLTag;
  sTemp := PageProducer.Content;

  try
    for i := 1 to TagParams.Count-1 do
    begin
      sIncludeParamValue := TagParams.Values[TagParams.Names[i]];
      sIncludeParamName := TagParams.Names[i];
      sIncludeParamValue := StringReplace(sIncludeParamValue, '[', '<', [rfReplaceAll]);
      sIncludeParamValue := StringReplace(sIncludeParamValue, ']', '>', [rfReplaceAll]);
      Result.Add(sIncludeParamName + '=' + sIncludeParamValue);
    end;
  finally
    PageProducer.Free;
  end;
  except
    FreeAndNil(Result);
  end;
end;


constructor TJvgTagParser.Create;
begin
  TagParams := TStringList.Create;
  AttributeFilter := TStringList.Create;
end;

destructor TJvgTagParser.Destroy;
begin
  TagParams.Free;
  AttributeFilter.Free;
  inherited;
end;

procedure TJvgTagParser.OnHTMLTag(Sender: TObject; Tag: TTag; const TagString: String; TagParams: TStrings; var ReplaceText: String);
var
  i: integer;
begin
  TagParams.Text := LowerCase(TagParams.Text);

  with AttributeFilter do
  for i:=0 to pred(TagParams.Count) do
    if IndexOfName(TagParams.Names[i]) <> -1 then
    if Values[TagParams.Names[i]] = Values[TagParams.Names[i]] then
    begin
      self.TagParams.Assign(TagParams);
    end;
end;

end.
