{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStringListDataProvider.pas, released on --.

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

unit JvStringListDataProvider;

interface

uses
  Windows, SysUtils, Classes,
  JvDataProvider, JvDataProviderImpl;

type
  TJvStringsDataProvider = class(TJvCustomDataProvider)
  protected
    class function ItemsClass: TJvDataItemsClass; override;

    function GetStrings: TStrings;
    procedure SetStrings(Value: TStrings);
  published
    property Strings: TStrings read GetStrings write SetStrings;
  end;

  { Supports text, imagelists, imageindex and subitems }
  TJvTreeDataProvider = class(TJvCustomDataProvider)
  protected
    class function PersistentDataItems: Boolean; override;
    class function ItemsClass: TJvDataItemsClass; override;
  published
    property Items;
  end;

implementation

uses
  ImgList, Graphics,
  JvTypes;

type
  TJvStringsDataItems = class(TJvBaseDataItems)
  private
    FItems: TStrings;
  protected
    procedure InitImplementers; override;
    function GetCount: Integer; override;
    function GetItem(I: Integer): IJvDataItem; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TJvStringsDataItemText = class(TJvBaseDataItemTextImpl)
  private
    FIndex: Integer;
    FStrings: TStrings;
  protected
    function GetCaption: string; override;
    procedure SetCaption(const Value: string); override;
  end;

  TJvStringsDataItem = class(TJvBaseDataItem)
  protected
    Impl: TJvStringsDataItemText;
    procedure InitID; override;
  public
    constructor Create(AItems: IJvDataItems; AStringList: TStrings; AIndex: Integer);
  end;

{ TJvStringsDataItems }

procedure TJvStringsDataItems.InitImplementers;
begin
  inherited InitImplementers;
  TJvCustomDataItemsRenderer.Create(Self);
end;

function TJvStringsDataItems.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvStringsDataItems.GetItem(I: Integer): IJvDataItem;
begin
  Result := TJvStringsDataItem.Create(Self, FItems, I);
end;

procedure TJvStringsDataItems.AfterConstruction;
begin
  inherited AfterConstruction;
  FItems := TStringList.Create;
end;

procedure TJvStringsDataItems.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FItems.Free;
end;

{ TJvStringsDataItem }

procedure TJvStringsDataItem.InitID; 
begin
  SetID(IntToHex(Impl.FIndex, 4));
end;

constructor TJvStringsDataItem.Create(AItems: IJvDataItems; AStringList: TStrings;
  AIndex: Integer);
begin
  inherited Create(AItems);
  Impl := TJvStringsDataItemText.Create(Self);
  Impl.FIndex := AIndex;
  Impl.FStrings := AStringList;
end;

{ TJvStringsDataItemText }

function TJvStringsDataItemText.GetCaption: string;
begin
  Result := FStrings[FIndex];
end;

procedure TJvStringsDataItemText.SetCaption(const Value: string);
begin
  FStrings[FIndex] := Value
end;

{ TJvStringsDataProvider }

class function TJvStringsDataProvider.ItemsClass: TJvDataItemsClass;
begin
  Result := TJvStringsDataItems;
end;

function TJvStringsDataProvider.GetStrings: TStrings;
begin
  Result := (DataItemsImpl as TJvStringsDataItems).FItems;
end;

procedure TJvStringsDataProvider.SetStrings(Value: TStrings);
begin
  Changing(pcrUpdateItems, GetItems);
  (DataItemsImpl as TJvStringsDataItems).FItems.Assign(Value);
  Changed(pcrUpdateItems, GetItems);
end;

type
  TJvTreeDataItems = class(TJvDataItemsList)
  protected
    procedure InitImplementers; override;
  public
    procedure BeforeDestruction; override;
  end;

  TJvTreeDataItemsDesigner = class(TJvDataItemsAggregatedObject, IJvDataItemsDesigner)
  protected
    function GetCount: Integer;
    function GetKind(Index: Integer; out Caption: string): Boolean;
    function NewByKind(Kind: Integer): IJvDataItem;
  end;

  TJvTreeDataItemsManagement = class(TJvBaseDataItemsListManagement)
  protected
    function New: IJvDataItem; override;
  end;

{ TJvTreeDataItems }

procedure TJvTreeDataItems.InitImplementers;
begin
  inherited InitImplementers;
  TJvCustomDataItemsRenderer.Create(Self);
  TJvTreeDataItemsManagement.Create(Self);
  TJvTreeDataItemsDesigner.Create(Self);
  if GetParent = nil then
    TJvCustomDataItemsImages.Create(Self);
end;

procedure TJvTreeDataItems.BeforeDestruction;
begin
  inherited;
end;

{ TJvTreeDataItemsDesigner }

function TJvTreeDataItemsDesigner.GetCount: Integer;
begin
  Result := 4;
end;

function TJvTreeDataItemsDesigner.GetKind(Index: Integer; out Caption: string): Boolean;
begin
  Result := True;
  case Index of
    0: Caption := 'Text only';
    1: Caption := 'Text and sub items';
    2: Caption := 'Text and image';
    3: Caption := 'Text, image and sub items';
    else Result := False;
  end;
end;

function TJvTreeDataItemsDesigner.NewByKind(Kind: Integer): IJvDataItem;
var
  Man: IJvDataItemsManagement;
begin
  if not Supports(Items, IJvDataItemsManagement, Man) then
    raise EJVCLException.Create('IJvDataItemsManagement interface is not supported.');
  case Kind of
    0:
      begin
        Result := Man.Add(TJvBaseDataItem.Create(Items));
        TJvDataItemTextImpl.Create(Result.Implementer as TJvBaseDataItem);
      end;
    1:
      begin
        Result := Man.Add(TJvBaseDataItem.Create(Items));
        TJvDataItemTextImpl.Create(Result.Implementer as TJvBaseDataItem);
        TJvTreeDataItems.CreateParent(Result);
      end;
    2:
      begin
        Result := Man.Add(TJvBaseDataItem.Create(Items));
        TJvDataItemTextImpl.Create(Result.Implementer as TJvBaseDataItem);
        TJvDataItemImageImpl.Create((Result.Implementer as TJvBaseDataItem));
      end;
    3:
      begin
        Result := Man.Add(TJvBaseDataItem.Create(Items));
        TJvDataItemTextImpl.Create(Result.Implementer as TJvBaseDataItem);
        TJvDataItemImageImpl.Create((Result.Implementer as TJvBaseDataItem));
        TJvTreeDataItems.CreateParent(Result);
      end;
    else raise EJVCLException.Create('Invalid item type requested.');
  end;
end;

{ TJvTreeDataItemsManagement }

function TJvTreeDataItemsManagement.New: IJvDataItem;
begin
  Result := TJvBaseDataItem.Create(Items);
  Add(Result);
end;

{ TJvTreeDataProvider }

class function TJvTreeDataProvider.PersistentDataItems: Boolean;
begin
  Result := True;
end;

class function TJvTreeDataProvider.ItemsClass: TJvDataItemsClass;
begin
  Result := TJvTreeDataItems;
end;

initialization
  RegisterClasses([TJvTreeDataItems, TJvTreeDataItemsDesigner, TJvTreeDataItemsManagement]);
end.
