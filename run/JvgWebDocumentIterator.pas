{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgWebDocumentIterator.PAS, released on 2003-01-15.

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

unit JvgWebDocumentIterator;

interface
uses windows, classes, sysutils, graphics, controls, menus, extctrls, JvgCommClasses, JvgTypes, shdocvw;

type
  {  TJvgIterator = class
      procedure First; virtual; abstract;
      procedure Next; virtual; abstract;
      function IsDone: boolean; virtual; abstract;
    end;}

  TJvgWebDocumentIterator = class {(TJvgIterator)}
  private
    WebBrowser: TWebBrowser;
    Doc, Item: variant;
    ItemIndex: integer;
    DocLocationHref: string;
    FCurrentItem: TFileName;
  public
    property CurrentItem: TFileName read FCurrentItem;

    constructor Create(WebBrowser: TWebBrowser);
    procedure First;
    procedure Next;
    function IsDone: boolean;
  end;

implementation

{ TJvgWebDocumentIterator }

constructor TJvgWebDocumentIterator.Create(WebBrowser: TWebBrowser);
begin
  self.WebBrowser := WebBrowser;
  Doc := WebBrowser.Document;
  DocLocationHref := Doc.Location.Href;
  DocLocationHref := StringReplace(DocLocationHref, '#', ' ', [rfReplaceAll]);
  DocLocationHref := StringReplace(DocLocationHref, 'file:///', '', [rfReplaceAll, rfIgnoreCase]);
  DocLocationHref := StringReplace(DocLocationHref, '/', '\', [rfReplaceAll]);
end;

procedure TJvgWebDocumentIterator.First;
begin
  ItemIndex := -1;
  Next;
end;

function TJvgWebDocumentIterator.IsDone: boolean;
begin
  Result := ItemIndex > Doc.images.length + {Doc.all.length +} Doc.links.length;
end;

procedure TJvgWebDocumentIterator.Next;
begin
  inc(ItemIndex);
  FCurrentItem := '';
  if IsDone then exit;

  try
    if ItemIndex <= Doc.images.length - 1 then
    begin
      Item := Doc.images.Item(ItemIndex);
      FCurrentItem := Item.src;
    end
    else if ItemIndex - Doc.images.length <= Doc.links.length - 1 then
    begin
      Item := Doc.links.Item(ItemIndex - Doc.images.length);
      FCurrentItem := Item.href;
    end
    else if ItemIndex - Doc.images.length - Doc.links.length <= Doc.all.length - 1 then
    begin
      Item := Doc.all.item(ItemIndex - Doc.images.length - Doc.links.length).style;
      FCurrentItem := Item.backgroundImage;
    end;
  except
    Next;
    exit;
  end;

  FCurrentItem := LowerCase(trim(FCurrentItem));
  if (FCurrentItem = '') and not IsDone then Next;

  if pos('#', FCurrentItem) > 0 then
    FCurrentItem := copy(FCurrentItem, 1, pos('#', FCurrentItem) - 1);

  FCurrentItem := StringReplace(FCurrentItem, 'file:///', '', [rfReplaceAll, rfIgnoreCase]);
  FCurrentItem := StringReplace(FCurrentItem, '/', '\', [rfReplaceAll]);

  if DocLocationHref = FCurrentItem then
  begin
    Next;
    exit;
  end;

  if pos('http:\\', FCurrentItem) = 1 then
  begin
    Next;
    exit;
  end;

  if pos('mailto:', FCurrentItem) = 1 then
  begin
    Next;
    exit;
  end;

  FCurrentItem := StringReplace(FCurrentItem, 'url(', '', [rfReplaceAll]);
  FCurrentItem := StringReplace(FCurrentItem, ')', '', [rfReplaceAll]);
  FCurrentItem := StringReplace(FCurrentItem, '%20', ' ', [rfReplaceAll]);
end;

end.
