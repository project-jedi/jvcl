{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

// (p3) simple unit that defines save/store interface
unit PersistSettings;

interface
uses
  SysUtils, Classes, IniFiles;

type
  EStorageHandlerError = class(Exception);
  // for convenience, alias TCustomIniFile here so users
  // don't all have to add IniFiles to their uses
  TPersistStorage = class(TCustomIniFile);

  // this interface should be implemente by whoever (form, component) can (and wants to) store
  // their settings (whatever that might be)
  IPersistSettings = interface
  ['{74727C9C-50F6-4C3A-8051-5CC5E64E9EB7}']
    procedure Load(Storage:TPersistStorage);
    procedure Save(Storage:TPersistStorage);
  end;
  
type
  TStorageHandlerFunction = function:TPersistStorage;
  TStorageHandlerMethod = function:TPersistStorage of object;

// call SetStorageHandler to set a function or method that creates and returns a
// TPersistStorage instance. NB! the function/method must be available throughout the
// life-time of the application! This can be accomplished by declaring
// a function in a globally available unit or a method in a class with
// application wide life-time (like the main form)
// You only need to call one of them, not both
procedure SetStorageHandler(AFunction:TStorageHandlerFunction);overload;
procedure SetStorageHandler(AMethod:TStorageHandlerMethod);overload;

// NB! before calling GetStorage, one of the SetStorageHandler procedures
// *must* have been set up with a valid Func parameter that is available
// throughout the life-time of the application (done with SetStorageHandler)!
function GetStorage:TPersistStorage;

// Save a component and any sub-component that implements the IPersistSettings interface
procedure SaveComponents(Root:TComponent;Storage:TPersistStorage);
// Load a component and any sub-component that implements the IPersistSettings interface
procedure LoadComponents(Root:TComponent;Storage:TPersistStorage);

// AutoSave calls GetStorage and then calls SaveComponents
procedure AutoSave(Root:TComponent);

// AutoLoad calls GetStorage and then calls LoadComponents
procedure AutoLoad(Root:TComponent);

implementation

var
  FFunction:TStorageHandlerFunction = nil;
  FMethod:TStorageHandlerMethod = nil;

procedure SetStorageHandler(AFunction:TStorageHandlerFunction);
begin
  FFunction := AFunction;
end;

procedure SetStorageHandler(AMethod:TStorageHandlerMethod);
begin
  FMethod := AMethod;
end;

function GetStorage:TPersistStorage;
begin
  if Assigned(FFunction) then
    Result := FFunction
  else if Assigned(FMethod) then
    Result := FMethod
  else
    raise EStorageHandlerError.Create('No handler assigned for GetStorage!');
end;

procedure SaveComponents(Root:TComponent;Storage:TPersistStorage);
var i:integer;PS:IPersistSettings;
begin
  if Supports(Root,IPersistSettings,PS) then
    PS.Save(Storage);
  for i := 0 to Root.ComponentCount-1 do
    SaveComponents(Root.Components[i],Storage);
end;

procedure LoadComponents(Root:TComponent;Storage:TPersistStorage);
var i:integer;PS:IPersistSettings;
begin
  if Supports(Root,IPersistSettings,PS) then
    PS.Load(Storage);
  for i := 0 to Root.ComponentCount-1 do
    LoadComponents(Root.Components[i],Storage);
end;

procedure AutoSave(Root:TComponent);
var Storage:TPersistStorage;
begin
  Storage := GetStorage;
  try
    SaveComponents(Root,Storage);
    Storage.UpdateFile;
  finally
    Storage.Free;
  end;
end;

procedure AutoLoad(Root:TComponent);
var Storage:TPersistStorage;
begin
  Storage := GetStorage;
  try
    LoadComponents(Root,Storage);
  finally
    Storage.Free;
  end;
end;

end.
