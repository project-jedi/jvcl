{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStringListToHtml.PAS, released on 2001-02-28.

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

unit JvStringListToHtml;

interface

uses
  SysUtils, Classes,
  JvComponent;

type
  TJvStringListToHtml = class(TJvComponent)
  private
    FStrings, FHTML: TStrings;
    FHTMLTitle: string;
    FHTMLLineBreak: string;
    function GetHTML: TStrings;
    procedure SetStrings(const Value: TStrings);
    procedure DoStringsChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ConvertToHtml(Source: TStrings; const Filename: string);
    procedure ConvertToHTMLStrings(Source,Destination: TStrings);
  published
    property HTML: TStrings read GetHTML;
    property Strings: TStrings read FStrings write SetStrings;
    property HTMLLineBreak:string read FHTMLLineBreak write FHTMLLineBreak;
    property HTMLTitle:string read FHTMLTitle write FHTMLTitle;
    property IncludeHeader:boolean read FIncludeHeader write FIncludeHeader default true;
  end;

implementation

procedure ConvertStringsToHTML(Source,Destination:TStrings;const HTMLTitle, HTMLLineBreak:string;IncludeHeader:boolean);
var
  I: integer;
begin
  if (Result = nil) or (Value = nil) then Exit;
  Result.BeginUpdate;
  Value.BeginUpdate;
  try
    if IncludeHeader then
    begin
      Result.Add('<HTML><HEAD>');
      Result.Add('<TITLE>' + HTMLTitle + '</TITLE></HEAD>');
      Result.Add('<BODY>');
    end;
    for I := 0 to Value.Count - 1 do
      Result.Add(Value[I] + HTMLLineBreak);
    if IncludeHeader then
    begin
      Result.Add('</BODY>');
      Result.Add('</HTML>');
    end;
  finally
    Value.EndUpdate;
    Result.EndUpdate;
  end;
end;

procedure TJvStringListToHtml.ConvertToHtml(Source: TStrings; const Filename: string);
var
  Dest: TStringlist;
begin
  if Source = nil then Exit;
  Dest := TStringlist.Create;
  try
    ConvertStringsToHTML(Source, Dest, HTMLTitile, HTMLLineBreak, true);
    Dest.SaveToFile(Filename);
  finally
    Dest.Free;
  end;
end;

procedure TJvStringListToHtml.ConvertToHTMLStrings(Source,Destination: TStrings);
begin
  ConvertStringsToHTML(Source,Destination,HTMLTitle, HTMLLineBreak, IncludeHeader);
end;

constructor TJvStringListToHtml.Create(AOwner: TComponent);
begin
  inherited;
  FStrings := TStringlist.Create;
  TStringlist(FStrings).OnChange := DoStringsChange;
  FHTMLLineBreak := '<BR>';
  FHTMLTitle := 'Converted by TJvStringListToHtml';
end;

destructor TJvStringListToHtml.Destroy;
begin
  FreeAndNil(FStrings);
  FreeAndNil(FHTML);
  inherited;
end;

procedure TJvStringListToHtml.DoStringsChange(Sender: TObject);
begin
  FreeAndNil(FHTML);
end;

function TJvStringListToHtml.GetHTML: TStrings;
begin
  if FHTML = nil then
  begin
    FHTML := TStringlist.Create;
    ConvertToHtmlStrings(Strings,FHTML);
  end;
  Result := FHTML;
end;

procedure TJvStringListToHtml.SetStrings(const Value: TStrings);
begin
  FStrings.Assign(Value);
end;

end.

