{******************************************************************}
{                                                                  }
{       Project JEDI                                               }
{       OS independent Dynamic Loading Helpers                     }
{                                                                  }
{ The initial developer of the this code is                        }
{ Robert Marquardt <robert_marquardt@gmx.de)                       }
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

interface

{$WEAKPACKAGEUNIT ON}

// each OS gets its own IFDEFed complete code block to make reading easier

{$IFDEF WIN32}

uses
  Windows;

type
  // Handle to a loaded DLL
  TModuleHandle = HINST;

const
  // Value designating an unassigned TModuleHandle od a failed loading
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(0);

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
procedure UnloadModule(var Module: TModuleHandle);
function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
{$ENDIF}

{$IFDEF LINUX}

uses
  Types, Libc;

type
  // Handle to a loaded .so
  TModuleHandle = Pointer;
  HModule = Pointer;

const
  // Value designating an unassigned TModuleHandle od a failed loading
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(nil);

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
procedure UnloadModule(var Module: TModuleHandle);
function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;

{$ENDIF}

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
  TModuleLoader = class
  private
    FHandle: HModule;
    FDLLName: string;
    function GetLoaded: boolean;
  protected
    procedure Load(LoadMethods:TModuleLoadMethods);virtual;
    procedure Unload;virtual;
    procedure Error(ErrorCode:Cardinal);virtual;
  public
    // Check whether a DLL (and optionally a function) is available on the system
    // To only check the DLL, leave ProcName empty
    class function IsAvaliable(const ADLLName:string;const AProcName:string=''):boolean;
    constructor Create(const ADLLName:string; LoadMethods:TModuleLoadMethods=[]);
    destructor Destroy;override;
    // Get a pointer to a function in the DLL. Should be called as GetProcedure('Name',@FuncPointer);
    // Returns true if the function was found. Note that a call to GetProcAddress is only executed if AProc = nil
    function GetProcedure(const AName:string; var AProc:Pointer):boolean;
    // Returns a symbol exported from the DLL and puts it in Buffer.
    // Make sure AName is actually a symbol and not a function or this will crash horribly!
    function GetExportedSymbol(const AName:string;var Buffer;Size:integer):boolean;
    // Changes a symbol exported from the DLL into the value in Buffer.
    // The change is not persistent (it will get lost when the DLL is unloaded)
    // Make sure AName is actually a symbol and not a function or this will crash horribly!
    function SetExportedSymbol(const AName:string;var Buffer;Size:integer):boolean;

    property Loaded:boolean read GetLoaded;
    property DLLName:string read FDLLName;
    property Handle:HModule read FHandle;
  end;
  {$IFDEF WIN32}
  {$IFDEF MODULELOADER_TEST}
  // This is a simple example of dynamically loading user32.dll and accessing the
  // RegisterShellHookWindow/DeregisterShellHookWindow functions
  // (since these might not be available on all systems)
  // NB! Win32 only!!!
  TRegisterShellHookWindowFunc = function(hWnd: HWND): BOOL; stdcall;
  TRegisterHookModuleLoader = class(TModuleLoader)
  private
    FRegisterShellHookWindow: TRegisterShellHookWindowFunc;
    FDeregisterShellHookWindow: TRegisterShellHookWindowFunc;
  protected
    procedure Load(LoadMethods:TModuleLoadMethods);override;
    procedure Unload;override;
  public
    constructor Create;
    // alias the DLL functions as methods
    function RegisterShellHookWindow(hWnd:HWND):BOOL;
    function DeregisterShellHookWindow(hWnd:HWND):BOOL;
  end;
  {$ENDIF MODULELOADER_TEST}
  {$ENDIF WIN32}

implementation

{$IFDEF WIN32 }
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
// as an extra the boolean variable Accu is updated
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

{$ENDIF}

{$IFDEF LINUX }

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
// as an extra the boolean variable Accu is updated
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

{$ENDIF}
// support routines for the TModuleloader class
function InternalLoadLibraryEx(const lpLibFilename: PChar; hFile: HFILE; dwFlags: DWORD): HModule;
begin
  {$IFDEF LINUX}
  Result := dlopen(lpLibFilename,dwFlags);
  {$ENDIF}
  {$IFDEF WIN32}
  Result := LoadLibraryEx(lpLibFilename,hFile,dwFlags);
  {$ENDIF}
end;

function InternalGetProcAddress(hModule: HModule;
  lpProcName: PChar): Pointer;
begin
  {$IFDEF LINUX}
  Result := dlsym(hModule,lpProcName);
  {$ENDIF}
  {$IFDEF WIN32}
  Result := GetProcAddress(hModule,lpProcName);
  {$ENDIF}
end;

function InternalFreeLibrary(hModule: HModule): BOOL;
begin
  {$IFDEF LINUX}
  Result := dlclose(hModule) = 0;
  {$ENDIF}
  {$IFDEF WIN32}
  Result := FreeLibrary(hModule);
  {$ENDIF}
end;

{ TModuleLoader }

constructor TModuleLoader.Create(const ADLLName: string; LoadMethods:TModuleLoadMethods=[]);
begin
  inherited Create;
  FDLLName := ADLLName;
  Load(LoadMethods);
end;

destructor TModuleLoader.Destroy;
begin
  Unload;
  inherited;
end;

procedure TModuleLoader.Error(ErrorCode: Cardinal);
begin
  // overriden classes should handle this
end;

function TModuleLoader.GetExportedSymbol(const AName: string; var Buffer;
  Size: integer): boolean;
var ASymbol:Pointer;
begin
  Result := GetProcedure(AName,ASymbol);
  if Result then
    Move(ASymbol^,Buffer,Size);
end;

function TModuleLoader.GetLoaded: boolean;
begin
  Result := Handle <> 0;
end;

function TModuleLoader.GetProcedure(const AName: string; var AProc: Pointer): boolean;
begin
  Result := Loaded;
  if Result and not Assigned(AProc) then
  begin
    AProc := InternalGetProcAddress(Handle,PChar(AName));
    Result := Assigned(AProc);
  end;
  if not Result then
  begin
    AProc := nil;
    Error(DWORD(TYPE_E_ELEMENTNOTFOUND));
  end;
end;

class function TModuleLoader.IsAvaliable(const ADLLName: string;const AProcName:string=''): boolean;
var FModule:HModule;P:Pointer;
begin
  FModule := InternalLoadLibraryEx(PChar(ADLLName),0,0);
  Result := FModule <> 0;
  if Result then
  begin
    if AProcName <> '' then
    begin
      P := InternalGetProcAddress(FModule,PChar(AProcName));
      Result := Assigned(P);
    end;
    InternalFreeLibrary(FModule);
  end;
end;

procedure TModuleLoader.Load(LoadMethods:TModuleLoadMethods);
const
  cLoadMethods:array[TModuleLoadMethod] of DWORD = (DONT_RESOLVE_DLL_REFERENCES,LOAD_LIBRARY_AS_DATAFILE,LOAD_WITH_ALTERED_SEARCH_PATH);
var Flags:DWORD;i:TModuleLoadMethod;
begin
  Flags := 0;
  for i := Low(TModuleLoadMethod) to High(TModuleLoadMethod) do
    if i in LoadMethods then
      Flags := Flags or cLoadMethods[i];
  if FHandle = 0 then
    FHandle := InternalLoadLibraryEx(PChar(DLLName),0,Flags);
  if FHandle = 0 then
    Error(GetLastError);
end;

function TModuleLoader.SetExportedSymbol(const AName: string; var Buffer;
  Size: integer): boolean;
var ASymbol:Pointer;
begin
  Result := GetProcedure(AName,ASymbol);
  if Result then
    Move(Buffer,ASymbol^,Size);
end;

procedure TModuleLoader.Unload;
begin
  if FHandle <> 0 then
    InternalFreeLibrary(FHandle);
  FHandle := 0;
end;

{$IFDEF WIN32}
{$IFDEF MODULELOADER_TEST}
{ TRegisterHookModuleLoader }

constructor TRegisterHookModuleLoader.Create;
begin
  // Don't allow users to change the DLL loaded:
  inherited Create('user32.dll');
end;

function TRegisterHookModuleLoader.DeregisterShellHookWindow(hWnd: HWND): BOOL;
begin
  // Load example 1: load the function once at request
  if Assigned(FDeregisterShellHookWindow) or GetProcedure('DeregisterShellHookWindow',@FDeregisterShellHookWindow) then
    Result := FDeregisterShellHookWindow(hWnd)
  else
    Result := false;
end;

procedure TRegisterHookModuleLoader.Load(LoadMethods: TModuleLoadMethods);
begin
  inherited Load(LoadMethods);
  // Load example 2: load the function once at startup
  GetProcedure('RegisterShellHookWindow',@FRegisterShellHookWindow);
end;

function TRegisterHookModuleLoader.RegisterShellHookWindow(
  hWnd: HWND): BOOL;
begin
  if Assigned(FRegisterShellHookWindow) then
    Result := FRegisterShellHookWindow(hWnd)
  else
    Result := false;
end;

procedure TRegisterHookModuleLoader.Unload;
begin
  inherited Unload;
  @FRegisterShellHookWindow := nil;
  @FDeregisterShellHookWindow := nil;
end;
{$ENDIF MODULELOADER_TEST}
{$ENDIF WIN32}


end.
