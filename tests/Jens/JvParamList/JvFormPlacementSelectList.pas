{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPlacemnt.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFormPlacementSelectList;

interface

uses
  Classes,
  JvAppStore, JvFormPlacement, JvAppStoreSelectList;

type
  TJvFormStorageSelectList = class (TJvAppStoreSelectList)
  private
    FFormStorage: TJvFormStorage;
  protected
    function GetFormStorage: TJvFormStorage; virtual;
    procedure SetFormStorage(Value: TJvFormStorage); virtual;
    function GetAppStore: TJvCustomAppStore; override;
    procedure SetAppStore(Value: TJvCustomAppStore); override;
  public
 //        constructor Create(AOwner: TComponent); override;
 //        destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RestoreFormStorage(ACaption: string = '');
    procedure SaveFormStorage(ACaption: string = '');
  published
    property FormStorage: TJvFormStorage read GetFormStorage write SetFormStorage;
  end;

implementation

uses
  SysUtils;

 //constructor tJvAppStoreSelectList.Create(AOwner: TComponent);
 //begin
 //  inherited Create(AOwner);
 //  FSelectList := TStringList.Create;
 //  FCheckEntries := True;
 //end;
 //
 //destructor TJvAppStoreSelectList.Destroy;
 //begin
 //  FreeAndNil(FSelectList);
 //  FreeAndNil(FSelectDialog);
 //  inherited Destroy;
 //end;

function TJvFormStorageSelectList.GetFormStorage: TJvFormStorage;
begin
  Result := FFormStorage;
end;

procedure TJvFormStorageSelectList.SetFormStorage(Value: TJvFormStorage);
begin
  FFormStorage := Value;
end;

function TJvFormStorageSelectList.GetAppStore: TJvCustomAppStore;
begin
  if Assigned(FFormStorage) then
    Result := FFormStorage.AppStorage
  else
    Result := nil;
end;

procedure TJvFormStorageSelectList.SetAppStore(Value: TJvCustomAppStore);
begin
  if Assigned(FFormStorage) then
    FFormStorage.AppStorage := Value;
end;

procedure TJvFormStorageSelectList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFormStorage) then
    FFormStorage := nil;
end;

procedure TJvFormStorageSelectList.RestoreFormStorage(ACaption: string = '');
begin
  if Assigned(FormStorage) then
  begin
    FormStorage.AppStoragePath := GetSelectPath(sloLoad, ACaption);
    if FormStorage.AppStoragePath <> '' then
      FormStorage.RestoreFormPlacement;
  end;
end;

procedure TJvFormStorageSelectList.SaveFormStorage(ACaption: string = '');
begin
  if Assigned(FormStorage) then
  begin
    FormStorage.AppStoragePath := GetSelectPath(sloStore, ACaption);
    if FormStorage.AppStoragePath <> '' then
      FormStorage.SaveFormPlacement;
  end;
end;

end.
