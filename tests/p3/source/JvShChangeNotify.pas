unit JvShChangeNotify;

interface
uses
  Windows, ShlObj;

type
  PPItemIDList = ^PItemIDList;
  TSHChangeNotifyEntry = packed record
    pidl:PItemIDList;
    Recursive:LongBool;
  end;

  TWMShellNotify = packed record
    Msg: Cardinal;
    ppidl: PPItemIDList;
    lEvent: DWORD;
    Result: Longint;
  end;

  TShChangeNotifyRegister = function(hWnd: HWND; fSources: integer; fEvents: DWORD; wMsg: UINT; cEntries: integer; var pfsne: TSHChangeNotifyEntry): ULONG; stdcall;
  TShChangeNotifyDeregister = function(ulID: ULONG): BOOL; stdcall;
  TSHChangeNotificationLock = function(hChange: THandle; dwProcessID: DWORD; var pppidl: PPItemIDList; var plEvent: DWORD): THandle; stdcall;
  TSHChangeNotificationUnlock = function(hLock: THandle): BOOL; stdcall;
  
var
  SHChangeNotifyRegister:TSHChangeNotifyRegister = nil;
  SHChangeNotifyDeregister:TSHChangeNotifyDeregister = nil;
  SHChangeNotification_Lock:TSHChangeNotificationLock = nil;
  SHChangeNotification_Unlock:TSHChangeNotificationUnlock = nil;


function LoadShChangeDLL: Boolean;
function IsShChangeLoaded:boolean;
procedure UnloadShChangeDLL;

implementation
const
  cShell32 = 'shell32.dll';
var
  ShellDLLHandle:HMODULE = 0;

function LoadShChangeDLL: Boolean;
begin
  if ShellDLLHandle = 0 then
    ShellDLLHandle := LoadLibrary(PChar(cShell32));
  if ShellDLLHandle <> 0 then
  begin
    SHChangeNotifyRegister := GetProcAddress(ShellDLLHandle, PChar(2));
    SHChangeNotifyDeRegister := GetProcAddress(ShellDLLHandle, PChar(4));
    SHChangeNotification_Lock := GetProcAddress(ShellDLLHandle, PChar(644));
    SHChangeNotification_Unlock := GetProcAddress(ShellDLLHandle, PChar(645));
    Result := Assigned(SHChangeNotifyRegister) and Assigned(SHChangeNotification_Lock);
  end
  else
    Result := False;
end;

function IsShChangeLoaded:boolean;
begin
  Result := (ShellDLLHandle <> 0) and Assigned(SHChangeNotifyRegister);
end;

procedure UnloadShChangeDLL;
begin
  if ShellDLLHandle <> 0 then
    FreeLibrary(ShellDLLHandle);
  SHChangeNotifyRegister := nil;
  SHChangeNotifyDeRegister := nil;
  SHChangeNotification_Lock := nil;
  SHChangeNotification_Unlock := nil;
  ShellDLLHandle := 0;
end;

end.
