{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgWebDocumentIterator.PAS, released on 2003-01-15.

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

unit JvgWebDocumentIterator;

{$I jvcl.inc}

interface

uses
  Windows, Classes, SysUtils, Graphics, Controls, Menus, ExtCtrls, SHDocVw,
  JvgCommClasses, JvgTypes;

type
  {  TJvgIterator = class(TObject)
     protected
       procedure First; virtual; abstract;
       procedure Next; virtual; abstract;
       function IsDone: Boolean; virtual; abstract;
    end;}

  TJvgWebDocumentIterator = class(TObject) // TJvgIterator
  private
    FWebBrowser: TWebBrowser;
    FDoc: Variant;
    FItem: Variant;
    FItemIndex: Integer;
    FDocLocationHRef: string;
    FCurrentItem: TFileName;
  public
    constructor Create(WebBrowser: TWebBrowser);
    procedure First;
    procedure Next;
    function IsDone: Boolean;
    property CurrentItem: TFileName read FCurrentItem;
  end;

implementation

constructor TJvgWebDocumentIterator.Create(WebBrowser: TWebBrowser);
begin
  inherited Create;
  FWebBrowser := WebBrowser;
  FDoc := WebBrowser.Document;
  FDocLocationHRef := FDoc.Location.HRef;
  FDocLocationHRef := StringReplace(FDocLocationHRef, '#', ' ', [rfReplaceAll]);
  FDocLocationHRef := StringReplace(FDocLocationHRef, 'file:///', '', [rfReplaceAll, rfIgnoreCase]);
  FDocLocationHRef := StringReplace(FDocLocationHRef, '/', '\', [rfReplaceAll]);
end;

procedure TJvgWebDocumentIterator.First;
begin
  FItemIndex := -1;
  Next;
end;

function TJvgWebDocumentIterator.IsDone: Boolean;
begin
  Result := FItemIndex > FDoc.Images.Length + {FDoc.All.Length +} FDoc.Links.Length;
end;

procedure TJvgWebDocumentIterator.Next;
begin
  Inc(FItemIndex);
  FCurrentItem := '';
  if IsDone then
    Exit;

  try
    if FItemIndex <= FDoc.Images.Length - 1 then
    begin
      FItem := FDoc.Images.Item(FItemIndex);
      FCurrentItem := FItem.Src;
    end
    else
    if FItemIndex - FDoc.Images.Length <= FDoc.Links.Length - 1 then
    begin
      FItem := FDoc.Links.Item(FItemIndex - FDoc.Images.Length);
      FCurrentItem := FItem.HRef;
    end
    else
    if FItemIndex - FDoc.Images.Length - FDoc.Links.Length <= FDoc.All.Length - 1 then
    begin
      FItem := FDoc.All.Item(FItemIndex - FDoc.Images.Length - FDoc.Links.Length).Style;
      FCurrentItem := FItem.BackgroundImage;
    end;
  except
    Next;
    Exit;
  end;

  FCurrentItem := LowerCase(Trim(FCurrentItem));
  if (FCurrentItem = '') and not IsDone then
    Next;

  if Pos('#', FCurrentItem) > 0 then
    FCurrentItem := Copy(FCurrentItem, 1, Pos('#', FCurrentItem) - 1);

  FCurrentItem := StringReplace(FCurrentItem, 'file:///', '', [rfReplaceAll, rfIgnoreCase]);
  FCurrentItem := StringReplace(FCurrentItem, '/', '\', [rfReplaceAll]);

  if (FDocLocationHRef = FCurrentItem) or
    (Pos('http:\\', FCurrentItem) = 1) or
    (Pos('mailto:', FCurrentItem) = 1) then
    Next
  else
  begin
    FCurrentItem := StringReplace(FCurrentItem, 'url(', '', [rfReplaceAll]);
    FCurrentItem := StringReplace(FCurrentItem, ')', '', [rfReplaceAll]);
    FCurrentItem := StringReplace(FCurrentItem, '%20', ' ', [rfReplaceAll]);
  end;
end;

end.

