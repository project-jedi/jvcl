{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgTagParser.PAS, released on 2003-01-15.

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

unit JvgTagParser;

{$I jvcl.inc}

interface

uses
  Classes, SysUtils,
  {$IFDEF COMPILER6_UP}
  HTTPApp, HTTPProd;
  {$ELSE}
  HTTPApp;
  {$ENDIF COMPILER6_UP}

type
  TJvgTagParser = class(TObject)
  private
    FTagParams: TStringList;
    FAttributeFilter: TStringList;
    function GetAttributeFilter: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    function Attributes(const STag: string): TStrings;
    procedure OnHTMLTag(Sender: TObject; Tag: TTag; const TagString: string; TagParams: TStrings; var ReplaceText: string);
    property AttributeFilter: TStrings read GetAttributeFilter;
  end;

implementation

constructor TJvgTagParser.Create;
begin
  inherited Create;
  FTagParams := TStringList.Create;
  FAttributeFilter := TStringList.Create;
end;

destructor TJvgTagParser.Destroy;
begin
  FTagParams.Free;
  FAttributeFilter.Free;
  inherited Destroy;
end;

function TJvgTagParser.GetAttributeFilter: TStrings;
begin
  Result := FAttributeFilter;
end;

function TJvgTagParser.Attributes(const STag: string): TStrings;
var
  I: Integer;
  PageProducer: TPageProducer;
  sTemp, sIncludeParamName, sIncludeParamValue: string;
begin
  Result := TStringList.Create;
  try
    PageProducer := TPageProducer.Create(nil);
    PageProducer.HTMLDoc.Text := StringReplace(STag, '<?', '<#', []);
    PageProducer.OnHTMLTag := OnHTMLTag;
    sTemp := PageProducer.Content;

    try
      for I := 1 to FTagParams.Count - 1 do
      begin
        sIncludeParamValue := FTagParams.Values[FTagParams.Names[I]];
        sIncludeParamName := FTagParams.Names[I];
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

procedure TJvgTagParser.OnHTMLTag(Sender: TObject; Tag: TTag; const TagString: string; TagParams: TStrings; var ReplaceText: string);
var
  I: Integer;
begin
  // (rom) completely silly
  TagParams.Text := LowerCase(TagParams.Text);
  with AttributeFilter do
    for I := 0 to TagParams.Count-1 do
      if IndexOfName(TagParams.Names[I]) <> -1 then
        if Values[TagParams.Names[I]] = Values[TagParams.Names[I]] then
          FTagParams.Assign(TagParams);
end;

end.
