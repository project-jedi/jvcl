{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStringListToHtml.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQStringListToHtml;

interface

uses
  SysUtils, Classes,
  JvQComponent;

type
  TJvStringListToHtml = class(TJvComponent)
  private
    FStrings: TStringList;
    FHTML: TStringList;
    FHTMLTitle: string;
    FHTMLLineBreak: string;
    FIncludeHeader: Boolean;
    function GetHTML: TStrings;
    function GetStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
    procedure DoStringsChange(Sender: TObject);
    procedure SetHTML(const Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ConvertToHtml(Source: TStrings; const FileName: string);
    procedure ConvertToHTMLStrings(Source, Destination: TStrings);
  published
    property HTML: TStrings read GetHTML write SetHTML stored False;
    property Strings: TStrings read GetStrings write SetStrings;
    property HTMLLineBreak: string read FHTMLLineBreak write FHTMLLineBreak;
    property HTMLTitle: string read FHTMLTitle write FHTMLTitle;
    property IncludeHeader: Boolean read FIncludeHeader write FIncludeHeader default True;
  end;

implementation

procedure ConvertStringsToHTML(Source, Destination: TStrings; const HTMLTitle, HTMLLineBreak: string; IncludeHeader: Boolean);
var
  I: Integer;
begin
  if (Source = nil) or (Destination = nil) then
    Exit;
  Destination.BeginUpdate;
  Source.BeginUpdate;
  try
    if IncludeHeader then
    begin
      Destination.Add('<HTML><HEAD>');
      Destination.Add('<TITLE>' + HTMLTitle + '</TITLE></HEAD>');
      Destination.Add('<BODY>');
    end;
    for I := 0 to Source.Count - 1 do
      Destination.Add(Source[I] + HTMLLineBreak);
    if IncludeHeader then
    begin
      Destination.Add('</BODY>');
      Destination.Add('</HTML>');
    end;
  finally
    Source.EndUpdate;
    Destination.EndUpdate;
  end;
end;

procedure TJvStringListToHtml.ConvertToHtml(Source: TStrings; const FileName: string);
var
  Dest: TStringList;
begin
  if Source = nil then
    Exit;
  Dest := TStringList.Create;
  try
    ConvertStringsToHTML(Source, Dest, HTMLTitle, HTMLLineBreak, True);
    Dest.SaveToFile(FileName);
  finally
    Dest.Free;
  end;
end;

procedure TJvStringListToHtml.ConvertToHTMLStrings(Source, Destination: TStrings);
begin
  ConvertStringsToHTML(Source, Destination, HTMLTitle, HTMLLineBreak, IncludeHeader);
end;

constructor TJvStringListToHtml.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStrings := TStringList.Create;
  FHTML := TStringList.Create;
  FStrings.OnChange := DoStringsChange;
  FHTMLLineBreak := '<BR>';
  FHTMLTitle := 'Converted by TJvStringListToHtml';
  FIncludeHeader := True;
end;

destructor TJvStringListToHtml.Destroy;
begin
  FreeAndNil(FStrings);
  FreeAndNil(FHTML);
  inherited Destroy;
end;

procedure TJvStringListToHtml.DoStringsChange(Sender: TObject);
begin
  FreeAndNil(FHTML);
end;

function TJvStringListToHtml.GetHTML: TStrings;
begin
  if ComponentState * [csLoading, csDestroying] <> [] then
    if FHTML.Count = 0 then
      ConvertToHtmlStrings(Strings, FHTML);
  Result := FHTML;
end;

procedure TJvStringListToHtml.SetHTML(const Value: TStrings);
begin
  // do nothing
end;

function TJvStringListToHtml.GetStrings: TStrings;
begin
  Result := FStrings;
end;

procedure TJvStringListToHtml.SetStrings(const Value: TStrings);
begin
  FStrings.Assign(Value);
  FHTML.Clear;
end;

end.

