{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFontDataProvider.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):

Last Modified: 2003-06-20

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFontDataProvider;

interface

uses
  Windows, SysUtils, Classes,
  JvDataProvider, JvDataProviderImpl;

type
  TJvFontDataProvider = class(TJvCustomDataProvider)
  protected
    class function ItemsClass: TJvDataItemsClass; override;
  end;

implementation

uses
  Forms, Graphics;

type
  TJvFontItems = class(TJvBaseDataItems)
  protected
    procedure InitImplementers; override;
    function GetCount: Integer; override;
    function GetItem(I: Integer): IJvDataItem; override;
  end;

  TJvFontItemText = class(TJvBaseDataItemTextImpl)
  private
    FIndex: Integer;
  protected
    function GetCaption: string; override;
    procedure SetCaption(const Value: string); override;
  end;

  TJvFontItem = class(TJvBaseDataItem)
  protected
    Impl: TJvFontItemText;
    procedure InitID; override;
  public
    constructor Create(AItems: IJvDataItems; const Index: Integer);
  end;

{ TJvFontItems }

procedure TJvFontItems.InitImplementers;
begin
  inherited InitImplementers;
  TJvCustomDataItemsTextRenderer.Create(Self);
end;

function TJvFontItems.GetCount: Integer;
begin
  Result := Screen.Fonts.Count;
end;

function TJvFontItems.GetItem(I: Integer): IJvDataItem;
begin
  Result := TJvFontItem.Create(Self, I);
end;

{ TJvFontItem }

procedure TJvFontItem.InitID;
begin
  SetID(IntToHex(Impl.FIndex, 4));
end;

constructor TJvFontItem.Create(AItems: IJvDataItems; const Index: Integer);
begin
  inherited Create(AItems);
  Impl := TJvFontItemText.Create(Self);
  Impl.FIndex := Index;
end;

{ TJvFontItemText }

function TJvFontItemText.GetCaption: string;
begin
  Result := Screen.Fonts[FIndex];
end;

procedure TJvFontItemText.SetCaption(const Value: string);
begin
  raise Exception.Create('The font data provider is a read-only list.');
end;

{ TJvFontDataProvider }

class function TJvFontDataProvider.ItemsClass: TJvDataItemsClass;
begin
  Result := TJvFontItems;
end;

end.
