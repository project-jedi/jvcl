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

uses  Classes, JvAppStore,
  JvFormPlacement, JvAppStoreSelectList;

type
  tJvFormStorageSelectList = class (tJvAppStoreSelectList)
  private
    fFormStorage : TJvFormStorage;
  protected
    function GetFormStorage : TJvFormStorage; virtual;
    procedure SetFormStorage(Value : TJvFormStorage); virtual;
    function GetAppStore : TJvCustomAppStore; override;
    procedure SetAppStore(Value : TJvCustomAppStore); override;
  public
 //        constructor create (aOwner : TComponent); override;
 //        destructor destroy; override;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure RestoreFormStorage(aCaption : string = '');
    procedure SaveFormStorage(aCaption : string = '');
  published
    property FormStorage : TJvFormStorage Read GetFormStorage Write SetFormStorage;
  end;

implementation

uses SysUtils;

 //constructor tJvAppStoreSelectList.create (aOwner : TComponent);
 //begin
 //  Inherited Create (aOwner);
 //  fSelectList := tStringList.Create;
 //  fCheckEntries := True;
 //end;
 //
 //destructor tJvAppStoreSelectList.destroy;
 //begin
 //  FreeAndNil (fSelectList);
 //  IF Assigned(fSelectDialog) THEN
 //    FreeAndNil (fSelectDialog);
 //  Inherited Destroy;
 //end;

function tJvFormStorageSelectList.GetFormStorage : TJvFormStorage;
begin
  Result := fFormStorage;
end;

procedure tJvFormStorageSelectList.SetFormStorage(Value : TJvFormStorage);
begin
  fFormStorage := Value;
end;

function tJvFormStorageSelectList.GetAppStore : TJvCustomAppStore;
begin
  if Assigned(fFormStorage) then
    Result := fFormStorage.AppStorage;
end;

procedure tJvFormStorageSelectList.SetAppStore(Value : TJvCustomAppStore);
begin
  if Assigned(fFormStorage) then
    fFormStorage.AppStorage := Value;
end;

procedure tJvFormStorageSelectList.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = fFormStorage) then
    fFormStorage := nil;
end;


procedure tJvFormStorageSelectList.RestoreFormStorage(aCaption : string = '');
begin
  if not Assigned(FormStorage) then
    Exit;
  FormStorage.AppStoragePath := GetSelectPath(sloLoad, aCaption);
  if FormStorage.AppStoragePath <> '' then
    FormStorage.RestoreFormPlacement;
end;

procedure tJvFormStorageSelectList.SaveFormStorage(aCaption : string = '');
begin
  if not Assigned(FormStorage) then
    Exit;
  FormStorage.AppStoragePath := GetSelectPath(sloStore, aCaption);
  if FormStorage.AppStoragePath <> '' then
    FormStorage.SaveFormPlacement;
end;


end.
