{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFillFontList.Pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):

Last Modified: 2003-04-23

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFillFontList;

interface

uses
  Windows, SysUtils, Classes,
  JvFillBasicImpl, JvFillIntf;

type
  TJvFontFiller = class(TJvCustomFiller)
  protected
    class function ItemsClass: TJvFillerItemsClass; override;
    function GetSupports: TJvFillerSupports; override;
    function GetOptionClass: TJvFillerOptionsClass; override;
  end;

  TFontFillerOptions = class(TJvFillerOptions)
  private
    FUseFontNames: Boolean;
  protected
    procedure SetUseFontNames(Value: Boolean);
  public
  published
    property UseFontNames: Boolean read FUseFontNames write SetUseFontNames;
  end;

implementation

uses
  Forms, Graphics;

type
  TJvFontItems = class(TJvBaseFillerItems)
  protected
    procedure InitImplementers; override;
    function GetCount: Integer; override;
    function GetItem(I: Integer): IFillerItem; override;
  end;

  TJvFontItemText = class(TJvBaseFillerTextItemImpl)
  private
    FIndex: Integer;
  protected
    function GetCaption: string; override;
    procedure SetCaption(const Value: string); override;
  end;

  TJvFontItem = class(TJvBaseFillerItem)
  protected
    Impl: TJvFontItemText;
    procedure InitID; override;
  public
    constructor Create(AItems: IFillerItems; const Index: Integer);
  end;

{ TJvFontItems }

procedure TJvFontItems.InitImplementers;
begin
  inherited InitImplementers;
  TJvCustomFillerItemsTextRenderer.Create(Self);
end;

function TJvFontItems.GetCount: Integer;
begin
  Result := Screen.Fonts.Count;
end;

function TJvFontItems.GetItem(I: Integer): IFillerItem;
begin
  Result := TJvFontItem.Create(Self, I);
end;

{ TJvFontItem }

procedure TJvFontItem.InitID;
begin
  SetID(IntToHex(Impl.FIndex, 4));
end;

constructor TJvFontItem.Create(AItems: IFillerItems; const Index: Integer);
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
  raise Exception.Create('Font filler is a read-only list.');
end;

{ TJvFontFiller }

class function TJvFontFiller.ItemsClass: TJvFillerItemsClass;
begin
  Result := TJvFontItems;
end;

function TJvFontFiller.GetSupports: TJvFillerSupports;
begin
  Result := [fsText, fsReadOnly, fsCanrender, fsCanMeasure];
end;

function TJvFontFiller.GetOptionClass: TJvFillerOptionsClass;
begin
  Result := TFontFillerOptions;
end;

{ TFontFillerOptions }

procedure TFontFillerOptions.SetUseFontNames(Value: Boolean);
begin
  if Value <> UseFontNames then
  begin
    FUseFontNames := Value;
    Changed;
  end;
end;

initialization
  RegisterClass(TFontFillerOptions);
end.
