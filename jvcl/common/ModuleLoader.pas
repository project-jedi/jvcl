{******************************************************************}
{                                                                  }
{       Project JEDI                                               }
{       OS independent Dynamic Loading Helpers                     }
{                                                                  }
{ The initial developer of the this code is                        }
{ Robert Marquardt <robert_marquardt att gmx dott de)              }
{                                                                  }
{ Copyright (C) 2000, 2001 Robert Marquardt.                       }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators (Project JEDI)              }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/NPL/NPL-1_1Final.html                     }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}

unit ModuleLoader;

{$I jvcl.inc}

{$WEAKPACKAGEUNIT ON}

interface

{$IFDEF MSWINDOWS}

uses
  Windows;

type
  // Handle to a loaded DLL
  TModuleHandle = HINST;

{$ENDIF MSWINDOWS}

{$IFDEF LINUX}
uses
  Types, Libc;

type
  // Handle to a loaded .so
  TModuleHandle = Pointer;

{$ENDIF LINUX}

const
  // Value designating an unassigned TModuleHandle or a failed loading
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(0);

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
procedure UnloadModule(var Module: TModuleHandle);
function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;

// (p3)
// Simple DLL loading class. The idea is to use it to dynamically load
// a DLL at run-time using the GetProcedure method. Another (better) use is to derive a
// new class for each DLL you are interested in and explicitly call GetProcedure for
// each function in an overriden Load method. You would then add procedure/function
// aliases to the new class that maps down to the internally managed function pointers.
// This class is built from an idea I read about in Delphi Magazine a while ago but
// I forget who was the originator. If you know, let me know and I'll put it in the credits

// NB!!!
// * Prepared for Kylix but not tested
// * Is GetLastError implemented on Kylix? RaiseLastOSError implies it is...

type
  TModuleLoadMethod = (ltDontResolveDllReferences, ltLoadAsDataFile, ltAlteredSearchPath);
  TModuleLoadMethods = set of TModuleLoadMethod;

  TModuleLoader = class(TObject)
  private
    FHandle: TModuleHandle;
    FDLLName: string;
    function GetLoaded: Boolean;
  protected
    procedure Load(LoadMethods: TModuleLoadMethods); virtual;
    procedure Unload; virtual;
    procedure Error(ErrorCode: Cardinal); virtual;
  public
    // Check whether a DLL (and optionally a function) is available on the system
    // To only check the DLL, leave ProcName empty
    class function IsAvaliable(const ADLLName: string; const AProcName: string = ''): Boolean;
    constructor Create(const ADLLName: string; LoadMethods: TModuleLoadMethods = []);
    destructor Destroy; override;
    // Get a pointer to a function in the DLL. Should be called as GetProcedure('Name',@FuncPointer);
    // Returns True if the function was found. Note that a call to GetProcAddress is only executed if AProc = nil
    function GetProcedure(const AName: string; var AProc: Pointer): Boolean;
    // Returns a symbol exported from the DLL and puts it in Buffer.
    // Make sure AName is actually a symbol and not a function or this will crash horribly!
    function GetExportedSymbol(const AName: string; var Buffer; Size: Integer): Boolean;
    // Changes a symbol exported from the DLL into the value in Buffer.
    // The change is not persistent (it will get lost when the DLL is unloaded)
    // Make sure AName is actually a symbol and not a function or this will crash horribly!
    function SetExportedSymbol(const AName: string; var Buffer; Size: Integer): Boolean;

    property Loaded: Boolean read GetLoaded;
    property DLLName: string read FDLLName;
    property Handle: TModuleHandle read FHandle;
  end;

implementation

{$IFDEF MSWINDOWS}
// load the DLL file FileName
// the rules for FileName are those of LoadLibrary
// Returns: True = success, False = failure to load
// Assigns: the handle of the loaded DLL to Module
// Warning: if Module has any other value than INVALID_MODULEHANDLE_VALUE
// on entry the function will do nothing but returning success.

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := LoadLibrary(PChar(FileName));
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;

// load the DLL file FileName
// LoadLibraryEx is used to get better control of the loading
// for the allowed values for flags see LoadLibraryEx documentation.

function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := LoadLibraryEx(PChar(FileName), 0, Flags);
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;

// unload a DLL loaded with LoadModule or LoadModuleEx
// The procedure will not try to unload a handle with
// value INVALID_MODULEHANDLE_VALUE and assigns this value
// to Module after unload.

procedure UnloadModule(var Module: TModuleHandle);
begin
  if Module <> INVALID_MODULEHANDLE_VALUE then
    FreeLibrary(Module);
  Module := INVALID_MODULEHANDLE_VALUE;
end;

// returns the pointer to the symbol named SymbolName
// if it is exported from the DLL Module
// nil is returned if the symbol is not available

function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := GetProcAddress(Module, PChar(SymbolName));
end;

// returns the pointer to the symbol named SymbolName
// if it is exported from the DLL Module
// nil is returned if the symbol is not available.
// as an extra the Boolean variable Accu is updated
// by anding in the success of the function.
// This is very handy for rendering a global result
// when accessing a long list of symbols.

function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := GetProcAddress(Module, PChar(SymbolName));
  Accu := Accu and (Result <> nil);
end;

// get the value of variables exported from a DLL Module
// Delphi cannot access variables in a DLL directly, so
// this function allows to copy the data from the DLL.
// Beware! You are accessing the DLL memory image directly.
// Be sure to access a variable not a function and be sure
// to read the correct amount of data.

function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
var
  Sym: Pointer;
begin
  Result := True;
  Sym := GetModuleSymbolEx(Module, SymbolName, Result);
  if Result then
    Move(Sym^, Buffer, Size);
end;

// set the value of variables exported from a DLL Module
// Delphi cannot access variables in a DLL directly, so
// this function allows to copy the data to the DLL!
// BEWARE! You are accessing the DLL memory image directly.
// Be sure to access a variable not a function and be sure
// to write the correct amount of data.
// The changes are not persistent. They get lost when the
// DLL is unloaded.

function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
var
  Sym: Pointer;
begin
  Result := True;
  Sym := GetModuleSymbolEx(Module, SymbolName, Result);
  if Result then
    Move(Buffer, Sym^, Size);
end;

{$ENDIF MSWINDOWS}

{$IFDEF LINUX}
const
  TYPE_E_ELEMENTNOTFOUND = $8002802B;

// load the .so file FileName
// the rules for FileName are those of dlopen()
// Returns: True = success, False = failure to load
// Assigns: the handle of the loaded .so to Module
// Warning: if Module has any other value than INVALID_MODULEHANDLE_VALUE
// on entry the function will do nothing but returning success.

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := dlopen(PChar(FileName), RTLD_NOW);
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;

// load the .so file FileName
// dlopen() with flags is used to get better control of the loading
// for the allowed values for flags see "man dlopen".

function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := dlopen(PChar(FileName), Flags);
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;

// unload a .so loaded with LoadModule or LoadModuleEx
// The procedure will not try to unload a handle with
// value INVALID_MODULEHANDLE_VALUE and assigns this value
// to Module after unload.

procedure UnloadModule(var Module: TModuleHandle);
begin
  if Module <> INVALID_MODULEHANDLE_VALUE then
    dlclose(Module);
  Module := INVALID_MODULEHANDLE_VALUE;
end;

// returns the pointer to the symbol named SymbolName
// if it is exported from the .so Module
// nil is returned if the symbol is not available

function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := dlsym(Module, PChar(SymbolName));
end;

// returns the pointer to the symbol named SymbolName
// if it is exported from the .so Module
// nil is returned if the symbol is not available.
// as an extra the Boolean variable Accu is updated
// by anding in the success of the function.
// This is very handy for rendering a global result
// when accessing a long list of symbols.

function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := dlsym(Module, PChar(SymbolName));
  Accu := Accu and (Result <> nil);
end;

// get the value of variables exported from a .so Module
// Delphi cannot access variables in a .so directly, so
// this function allows to copy the data from the .so.
// Beware! You are accessing the .so memory image directly.
// Be sure to access a variable not a function and be sure
// to read the correct amount of data.

function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
var
  Sym: Pointer;
begin
  Result := True;
  Sym := GetModuleSymbolEx(Module, SymbolName, Result);
  if Result then
    Move(Sym^, Buffer, Size);
end;

// set the value of variables exported from a .so Module
// Delphi cannot access variables in a .so directly, so
// this function allows to copy the data to the .so!
// BEWARE! You are accessing the .so memory image directly.
// Be sure to access a variable not a function and be sure
// to write the correct amount of data.
// The changes are not persistent. They get lost when the
// .so is unloaded.

function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
var
  Sym: Pointer;
begin
  Result := True;
  Sym := GetModuleSymbolEx(Module, SymbolName, Result);
  if Result then
    Move(Buffer, Sym^, Size);
end;

{$ENDIF LINUX}

//=== { TModuleLoader } ======================================================

constructor TModuleLoader.Create(const ADLLName: string; LoadMethods: TModuleLoadMethods = []);
begin
  inherited Create;
  FHandle := INVALID_MODULEHANDLE_VALUE;
  FDLLName := ADLLName;
  Load(LoadMethods);
end;

destructor TModuleLoader.Destroy;
begin
  Unload;
  inherited Destroy;
end;

procedure TModuleLoader.Error(ErrorCode: Cardinal);
begin
  // overriden classes should handle this
end;

function TModuleLoader.GetExportedSymbol(const AName: string; var Buffer;
  Size: Integer): Boolean;
var
  ASymbol: Pointer;
begin
  Result := GetProcedure(AName, ASymbol);
  if Result then
    Move(ASymbol^, Buffer, Size);
end;

function TModuleLoader.GetLoaded: Boolean;
begin
  Result := Handle <> INVALID_MODULEHANDLE_VALUE;
end;

function TModuleLoader.GetProcedure(const AName: string; var AProc: Pointer): Boolean;
begin
  Result := Loaded;
  if Result and not Assigned(AProc) then
  begin
    AProc := GetModuleSymbol(Handle, AName);
    Result := Assigned(AProc);
  end;
  if not Result then
  begin
    AProc := nil;
    Error(DWORD(TYPE_E_ELEMENTNOTFOUND));
  end;
end;

class function TModuleLoader.IsAvaliable(const ADLLName: string; const AProcName: string = ''): Boolean;
var
  Module: TModuleHandle;
  P: Pointer;
begin
  Result := LoadModule(Module, ADLLName);
  if Result then
  begin
    if AProcName <> '' then
    begin
      P := GetModuleSymbol(Module, AProcName);
      Result := Assigned(P);
    end;
    UnloadModule(Module);
  end;
end;

procedure TModuleLoader.Load(LoadMethods: TModuleLoadMethods);
const
  cLoadMethods: array [TModuleLoadMethod] of DWORD =
    {$IFDEF MSWINDOWS}
    (DONT_RESOLVE_DLL_REFERENCES, LOAD_LIBRARY_AS_DATAFILE, LOAD_WITH_ALTERED_SEARCH_PATH);
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    (RTLD_LAZY, RTLD_LAZY, RTLD_LAZY); // there is not really a equivalent under Linux
    {$ENDIF LINUX}
var
  Flags: DWORD;
  I: TModuleLoadMethod;
begin
  Flags := 0;
  for I := Low(TModuleLoadMethod) to High(TModuleLoadMethod) do
    if I in LoadMethods then
      Flags := Flags or cLoadMethods[I];
  if FHandle = INVALID_MODULEHANDLE_VALUE then
    LoadModuleEx(FHandle, DLLName, Flags);
  if FHandle = INVALID_MODULEHANDLE_VALUE then
    Error(GetLastError);
end;

function TModuleLoader.SetExportedSymbol(const AName: string; var Buffer;
  Size: Integer): Boolean;
var
  ASymbol: Pointer;
begin
  Result := GetProcedure(AName, ASymbol);
  if Result then
    Move(Buffer, ASymbol^, Size);
end;

procedure TModuleLoader.Unload;
begin
  if FHandle <> INVALID_MODULEHANDLE_VALUE then
    UnloadModule(FHandle);
  FHandle := INVALID_MODULEHANDLE_VALUE;
end;

end.

