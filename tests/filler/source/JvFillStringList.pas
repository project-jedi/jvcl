{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFillStringList.Pas, released on --.

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

unit JvFillStringList;

interface

uses
  Windows, SysUtils, Classes,
  JvFillBasicImpl, JvFillIntf;

type
  TJvStringsFiller = class(TJvCustomFiller)
  protected
    class function ItemsClass: TJvFillerItemsClass; override;
    function GetSupports: TJvFillerSupports; override;
    function GetOptionClass: TJvFillerOptionsClass; override;

    function GetStrings: TStrings;
    procedure SetStrings(Value: TStrings);
  published
    property Strings: TStrings read GetStrings write SetStrings;
  end;

  TJvTreeFillerTree = (TreeFillerTree);

  { Supports text, imagelists, imageindex and subitems }
  TJvTreeFiller = class(TJvCustomFiller)
  private
    FItems: TJvTreeFillerTree;
  protected
    class function PersistentFillerItems: Boolean; override;
    class function ItemsClass: TJvFillerItemsClass; override;
    function GetSupports: TJvFillerSupports; override;
    function GetOptionClass: TJvFillerOptionsClass; override;
  published
    property Items: TJvTreeFillerTree read FItems write FItems stored False;
  end;

implementation

uses
  ImgList, Graphics,
  JvTypes;

type
  TJvStringsFillerItems = class(TJvBaseFillerItems)
  private
    FItems: TStrings;
  protected
    procedure InitImplementers; override;
    function GetCount: Integer; override;
    function GetItem(I: Integer): IFillerItem; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TJvStringsFillerItemText = class(TJvBaseFillerTextItemImpl)
  private
    FIndex: Integer;
    FStrings: TStrings;
  protected
    function GetCaption: string; override;
    procedure SetCaption(const Value: string); override;
  end;

  TJvStringsFillerItem = class(TJvBaseFillerItem)
  protected
    Impl: TJvStringsFillerItemText;
    procedure InitID; override;
  public
    constructor Create(AItems: IFillerItems; AStringList: TStrings; AIndex: Integer);
  end;

{ TJvStringsFillerItems }

procedure TJvStringsFillerItems.InitImplementers;
begin
  inherited InitImplementers;
  TJvCustomFillerItemsTextRenderer.Create(Self);
end;

function TJvStringsFillerItems.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvStringsFillerItems.GetItem(I: Integer): IFillerItem;
begin
  Result := TJvStringsFillerItem.Create(Self, FItems, I);
end;

procedure TJvStringsFillerItems.AfterConstruction;
begin
  inherited AfterConstruction;
  FItems := TStringList.Create;
end;

procedure TJvStringsFillerItems.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FItems.Free;
end;

{ TJvStringsFillerItem }

procedure TJvStringsFillerItem.InitID; 
begin
  SetID(IntToHex(Impl.FIndex, 4));
end;

constructor TJvStringsFillerItem.Create(AItems: IFillerItems; AStringList: TStrings;
  AIndex: Integer);
begin
  inherited Create(AItems);
  Impl := TJvStringsFillerItemText.Create(Self);
  Impl.FIndex := AIndex;
  Impl.FStrings := AStringList;
end;

{ TJvStringsFillerItemText }

function TJvStringsFillerItemText.GetCaption: string;
begin
  Result := FStrings[FIndex];
end;

procedure TJvStringsFillerItemText.SetCaption(const Value: string);
begin
  if not (fsReadonly in Item.Items.Filler.GetSupports) then
    FStrings[FIndex] := Value
  else
    raise EJVCLException.Create('Filler is read only; you can''t change the item.');
end;

{ TJvStringsFiller }

class function TJvStringsFiller.ItemsClass: TJvFillerItemsClass;
begin
  Result := TJvStringsFillerItems;
end;

function TJvStringsFiller.GetSupports: TJvFillerSupports;
begin
  Result := [fsText, fsCanRender, fsCanMeasure];
end;

function TJvStringsFiller.GetOptionClass: TJvFillerOptionsClass;
begin
  Result := nil;
end;

function TJvStringsFiller.GetStrings: TStrings;
begin
  Result := (FillerItemsImpl as TJvStringsFillerItems).FItems;
end;

procedure TJvStringsFiller.SetStrings(Value: TStrings);
begin
  if fsReadonly in getSupports then
    raise EJVCLException.Create('Filler is marked read only; you can''t change the list.');
  Changing(frUpdate);
  (FillerItemsImpl as TJvStringsFillerItems).FItems.Assign(Value);
  Changed(frUpdate);
end;

type
  TJvTreeFillerItems = class(TJvFillerItemsList)
  protected
    procedure InitImplementers; override;
  public
    procedure BeforeDestruction; override;
  end;

  TJvTreeFillerItemsDesigner = class(TJvFillerItemsAggregatedObject, IFillerItemsDesigner)
  protected
    function GetCount: Integer;
    function GetKind(Index: Integer; out Caption: string): Boolean;
    function NewByKind(Kind: Integer): IFillerItem;
  end;

  TJvTreeFillerItemsManagment = class(TJvBaseFillerItemsListManagment)
  protected
    function New: IFillerItem; override;
  end;

{ TJvTreeFillerItems }

procedure TJvTreeFillerItems.InitImplementers;
begin
  inherited InitImplementers;
  TJvTreeFillerItemsManagment.Create(Self);
  TJvTreeFillerItemsDesigner.Create(Self);
  TJvCustomFillerItemsImages.Create(Self);
end;

procedure TJvTreeFillerItems.BeforeDestruction;
begin
  inherited;
end;

{ TJvTreeFillerItemsDesigner }

function TJvTreeFillerItemsDesigner.GetCount: Integer;
begin
  Result := 4;
end;

function TJvTreeFillerItemsDesigner.GetKind(Index: Integer; out Caption: string): Boolean;
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

function TJvTreeFillerItemsDesigner.NewByKind(Kind: Integer): IFillerItem;
var
  Man: IFillerItemsManagment;
begin
  if not Supports(Items, IFillerItemsManagment, Man) then
    raise EJVCLException.Create('IFillerItemManagment interface is not supported.');
  case Kind of
    0:
      begin
        Result := Man.Add(TJvBaseFillerItem.Create(Items));
        TJvFillerTextItemImpl.Create(Result.Implementer as TJvBaseFillerItem);
      end;
    1:
      begin
        Result := Man.Add(TJvBaseFillerItem.Create(Items));
        TJvFillerTextItemImpl.Create(Result.Implementer as TJvBaseFillerItem);
        TJvTreeFillerItems.CreateParent(Result);
      end;
    2:
      begin
        Result := Man.Add(TJvBaseFillerItem.Create(Items));
        TJvFillerTextItemImpl.Create(Result.Implementer as TJvBaseFillerItem);
        TJvFillerImageItemImpl.Create((Result.Implementer as TJvBaseFillerItem));
      end;
    3:
      begin
        Result := Man.Add(TJvBaseFillerItem.Create(Items));
        TJvFillerTextItemImpl.Create(Result.Implementer as TJvBaseFillerItem);
        TJvFillerImageItemImpl.Create((Result.Implementer as TJvBaseFillerItem));
        TJvTreeFillerItems.CreateParent(Result);
      end;
    else raise EJVCLException.Create('Invalid item type requested.');
  end;
end;

{ TJvTreeFillerItemsManagment }

function TJvTreeFillerItemsManagment.New: IFillerItem;
begin
  Result := TJvBaseFillerItem.Create(Items);
  Add(Result);
end;

{ TJvTreeFiller }

class function TJvTreeFiller.PersistentFillerItems: Boolean;
begin
  Result := True;
end;

class function TJvTreeFiller.ItemsClass: TJvFillerItemsClass;
begin
  Result := TJvTreeFillerItems;
end;

function TJvTreeFiller.GetSupports: TJvFillerSupports;
begin
  Result := [fsText, fsImages, fsImageIndex, fsCanRender, fsCanMeasure, fsSubItems];
end;

function TJvTreeFiller.GetOptionClass: TJvFillerOptionsClass;
begin
  Result := nil;
end;

initialization
  RegisterClasses([TJvTreeFillerItems, TJvTreeFillerItemsDesigner, TJvTreeFillerItemsManagment]);
end.
