(*----------------------------------------------------------------------------*
 *  DirectX 9 C++ common framework adaptation for Delphi by Alexey Barkovoy   *
 *  E-Mail: clootie@reactor.ru                                                *
 *                                                                            *
 *  Desc: Direct3D part of framework.                                         *
 *  Delphi versions 5-7 are supported                                         *
 *                                                                            *
 *  Modified: 11-Feb-2002                                                     *
 *                                                                            *
 *  Latest version can be downloaded from:                                    *
 *     http://clootie.narod.ru/delphi                                         *
 *----------------------------------------------------------------------------*)
//-----------------------------------------------------------------------------
// File: DXUtil.h DXUtil.cpp
//
// Desc: Helper functions and typing shortcuts for DirectX programming.
//
// Copyright (c) Microsoft Corporation. All rights reserved
//-----------------------------------------------------------------------------

unit DXUtil;

{$I DirectX.inc}

interface

uses Windows, D3DX9;

//-----------------------------------------------------------------------------
// Miscellaneous helper functions
//-----------------------------------------------------------------------------
//#define SAFE_DELETE(p)       { if(p) { delete (p);     (p)=NULL; } }
//#define SAFE_DELETE_ARRAY(p) { if(p) { delete[] (p);   (p)=NULL; } }
//#define SAFE_RELEASE(p)      { if(p) { (p)->Release(); (p)=NULL; } }

procedure SAFE_RELEASE(var i);
procedure SAFE_DELETE(var Obj);

{$IFNDEF UNDER_CE}
const
  // HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND)
  HRESULT_FROM_WIN32_ERROR_FILE_NOT_FOUND =
  HRESULT((ERROR_FILE_NOT_FOUND and $0000FFFF) or (FACILITY_WIN32 shl 16));
  // #define HRESULT_FROM_WIN32(x)   (x ? ((HRESULT) (((x) & 0x0000FFFF) | (FACILITY_WIN32 << 16) | 0x80000000)) : 0 )

//-----------------------------------------------------------------------------
// Name: DXUtil_GetDXSDKMediaPath() and DXUtil_FindMediaFile()
// Desc: Returns the DirectX SDK path, as stored in the system registry
//       during the SDK install.
//-----------------------------------------------------------------------------
function DXUtil_GetDXSDKMediaPathCch(strDest: PChar; cchDest: Integer): HRESULT;
function DXUtil_GetDXSDKMediaPathCb(szDest: PChar; cbDest: Integer): HRESULT;
function DXUtil_FindMediaFileCch(strDestPath: PChar; cchDest: Integer; strFilename: PChar): HRESULT;
function DXUtil_FindMediaFileCb(szDestPath: PChar; cbDest: Integer; strFilename: PChar): HRESULT;
{$ENDIF} // !UNDER_CE


//-----------------------------------------------------------------------------
// Name: DXUtil_Read*RegKey() and DXUtil_Write*RegKey()
// Desc: Helper functions to read/write a string registry key
//-----------------------------------------------------------------------------
function DXUtil_WriteStringRegKey(hKey_: HKEY; strRegName: PChar; strValue: PChar): HRESULT;
{function DXUtil_WriteIntRegKey (hKey_: HKEY; strRegName: PChar; dwValue: DWORD): HRESULT;
function DXUtil_WriteGuidRegKey(hKey_: HKEY; strRegName: PChar; guidValue: TGUID): HRESULT;
function DXUtil_WriteBoolRegKey(hKey_: HKEY; strRegName: PChar; bValue: BOOL): HRESULT;

function DXUtil_ReadStringRegKeyCch(hKey_: HKEY; strRegName: PChar; strDest: PChar; cchDest: DWORD; strDefault: PChar): HRESULT;
function DXUtil_ReadStringRegKeyCb(hKey_: HKEY; strRegName: PChar; strDest: PChar; cbDest: DWORD; strDefault: PChar): HRESULT;
function DXUtil_ReadIntRegKey (hKey_: HKEY; strRegName: PChar; var pdwValue: DWORD; dwDefault: DWORD): HRESULT;
function DXUtil_ReadGuidRegKey(hKey_: HKEY; strRegName: PChar; var pGuidValue: TGUID; guidDefault: TGUID): HRESULT;
function DXUtil_ReadBoolRegKey(hKey_: HKEY; strRegName: PChar; var pbValue: BOOL; bDefault: BOOL): HRESULT; }


//-----------------------------------------------------------------------------
// Name: DXUtil_Timer()
// Desc: Performs timer opertations. Use the following commands:
//          TIMER_RESET           - to reset the timer
//          TIMER_START           - to start the timer
//          TIMER_STOP            - to stop (or pause) the timer
//          TIMER_ADVANCE         - to advance the timer by 0.1 seconds
//          TIMER_GETABSOLUTETIME - to get the absolute system time
//          TIMER_GETAPPTIME      - to get the current time
//          TIMER_GETELAPSEDTIME  - to get the time that elapsed between
//                                  TIMER_GETELAPSEDTIME calls
//-----------------------------------------------------------------------------
type
  TIMER_COMMAND = DWORD;

const
  TIMER_RESET           = 0;
  TIMER_START           = 1;
  TIMER_STOP            = 2;
  TIMER_ADVANCE         = 3;
  TIMER_GETABSOLUTETIME = 4;
  TIMER_GETAPPTIME      = 5;
  TIMER_GETELAPSEDTIME  = 6;

function DXUtil_Timer(command: TIMER_COMMAND): Single; stdcall;


//-----------------------------------------------------------------------------
// UNICODE support for converting between CHAR, TCHAR, and WCHAR strings
//-----------------------------------------------------------------------------
//todo: Still TODO!!!
(*function DXUtil_ConvertAnsiStringToWideCch(wstrDestination: PWideChar; const strSource: PAnsiChar; cchDestChar: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = -1{$ENDIF}): HRESULT;
function DXUtil_ConvertWideStringToAnsiCch(strDestination: PAnsiChar; const wstrSource: PWideChar; cchDestChar: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = -1{$ENDIF}): HRESULT; *)
function DXUtil_ConvertGenericStringToAnsiCch(strDestination: PAnsiChar; const tstrSource: PChar; cchDestChar: Integer = -1): HRESULT;
(* function DXUtil_ConvertGenericStringToWideCch(wstrDestination: PWideChar; const tstrSource: PChar; cchDestChar: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = -1{$ENDIF}): HRESULT; *)
function DXUtil_ConvertAnsiStringToGenericCch(tstrDestination: PChar; const strSource: PAnsiChar; cchDestChar: Integer = -1): HRESULT;
(* function DXUtil_ConvertWideStringToGenericCch(tstrDestination: PChar; const wstrSource: PWideChar; cchDestChar: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = -1{$ENDIF}): HRESULT;
function DXUtil_ConvertAnsiStringToWideCb(wstrDestination: PWideChar; const strSource: PChar; cbDestChar: Integer): HRESULT;
function DXUtil_ConvertWideStringToAnsiCb(strDestination: PChar; const wstrSource: PWideChar; cbDestChar: Integer): HRESULT; *)
function DXUtil_ConvertGenericStringToAnsiCb(strDestination: PAnsiChar; const tstrSource: PChar; cbDestChar: Integer): HRESULT;
(* function DXUtil_ConvertGenericStringToWideCb(wstrDestination: PWideChar; const tstrSource: PChar; cbDestChar: Integer): HRESULT; *)
function DXUtil_ConvertAnsiStringToGenericCb(tstrDestination: PChar; const strSource: PChar; cbDestChar: Integer): HRESULT;
(* function DXUtil_ConvertWideStringToGenericCb(tstrDestination: PChar; const wstrSource: PWideChar; cbDestChar: Integer): HRESULT;
*)

//-----------------------------------------------------------------------------
// Readme functions
//-----------------------------------------------------------------------------
//procedure DXUtil_LaunchReadme(hWnd: THandle; strLoc: PChar{$IFDEF SUPPORTS_DEFAULTPARAMS} = nil{$ENDIF});

//-----------------------------------------------------------------------------
// GUID to String converting 
//-----------------------------------------------------------------------------
//function DXUtil_ConvertGUIDToStringCch(const pGuidIn: TGUID; strDest: PChar; cchDestChar: Integer): HRESULT;
//function DXUtil_ConvertGUIDToStringCb(const pGuidSrc: TGUID; strDest: PChar; cbDestChar: Integer): HRESULT;
// function DXUtil_ConvertStringToGUID(const strIn: PChar; out{var ?} pGuidOut: TGUID): BOOL;


//-----------------------------------------------------------------------------
// Debug printing support
// See dxerr9.h for more debug printing support
//-----------------------------------------------------------------------------
{VOID    DXUtil_Trace( TCHAR* strMsg, ... );

#if defined(DEBUG) | defined(_DEBUG)
    #define DXTRACE           DXUtil_Trace
#else
    #define DXTRACE           sizeof
#endif}


//-----------------------------------------------------------------------------
// Name: ArrayListType
// Desc: Indicates how data should be stored in a CArrayList
//-----------------------------------------------------------------------------
type
  TArrayListType = (
    AL_VALUE,       // entry data is copied into the list
    AL_REFERENCE    // entry pointers are copied into the list
  );


//-----------------------------------------------------------------------------
// Name: CArrayList
// Desc: A growable array
//-----------------------------------------------------------------------------
  CArrayList = class
  protected
    m_ArrayListType: TArrayListType;
    m_pData: Pointer;
    m_BytesPerEntry: Integer;
    m_NumEntries: Integer;
    m_NumEntriesAllocated: Integer;

  public
    constructor Create(_Type: TArrayListType; BytesPerEntry: Integer = 0);
    destructor Destroy; override;
    function Add(pEntry: Pointer): HRESULT;
    procedure Remove(Entry: Integer);
    function GetPtr(Entry: Integer): Pointer;
    function Contains(pEntryData: Pointer): Boolean;
    procedure Clear; { m_NumEntries = 0; }
    property Count: Integer read m_NumEntries; { return m_NumEntries; }
  end;

//-----------------------------------------------------------------------------
// WinCE build support
//-----------------------------------------------------------------------------

{$IFNDEF UNDER_CE}

// #define CheckDlgButton(hdialog, id, state) ::SendMessage(::GetDlgItem(hdialog, id), BM_SETCHECK, state, 0)
//procedure CheckDlgButton(hdialog: HWnd; id, state: LongWord);
// #define IsDlgButtonChecked(hdialog, id) ::SendMessage(::GetDlgItem(hdialog, id), BM_GETCHECK, 0L, 0L)
//function IsDlgButtonChecked(hdialog: HWnd; id: LongWord): Boolean;
//#define GETTIMESTAMP GetTickCount
function GETTIMESTAMP: DWORD;
//todo: _TWINCE ?
//#define _TWINCE(x) _T(x)

//function GetScrollPos(hWnd: HWND; nBar: Integer): Integer;
//__inline int GetScrollPos(HWND hWnd, int nBar)
(*{
	SCROLLINFO si;
	memset(&si, 0, sizeof(si));
	si.cbSize = sizeof(si);
	si.fMask = SIF_POS;
	if (!GetScrollInfo(hWnd, nBar, &si))
	{
		return 0;
	}
	else
	{
		return si.nPos;
	}
}*)

{$ELSE} // !UNDER_CE

#define GETTIMESTAMP timeGetTime
#define _TWINCE(x) x

{$ENDIF} // UNDER_CE

implementation

uses SysUtils;

procedure SAFE_RELEASE(var i);
begin
  if IUnknown(i) <> nil then IUnknown(i):= nil;
end;

procedure SAFE_DELETE(var Obj);
var
  Temp: TObject;
begin
  Temp:= TObject(Obj);
  Pointer(Obj):= nil;
  Temp.Free;
end;


type
  LPShellExecute = function(hwnd: HWND; lpOperation, lpFile, lpParameters,
                     lpDirectory: PChar; nShowCmd: Integer): HINST; stdcall;


{$IFNDEF UNDER_CE}
//-----------------------------------------------------------------------------
// Name: DXUtil_GetDXSDKMediaPathCch()
// Desc: Returns the DirectX SDK media path
//       cchDest is the size in TCHARs of strDest.  Be careful not to
//       pass in sizeof(strDest) on UNICODE builds.
//-----------------------------------------------------------------------------
function DXUtil_GetDXSDKMediaPathCch(strDest: PChar; cchDest: Integer): HRESULT;
var
  Key: HKEY;
  lResult: Longint;
  dwType: DWORD;
  dwSize: DWORD;
const
  strMedia: PChar = '\Media\';
begin
  if (strDest = nil) or (cchDest < 1) then
  begin
    Result:= E_INVALIDARG;
    Exit;
  end;

  StrCopy(strDest, '');
  Result:= E_FAIL;

  // Open the appropriate registry key
  lResult:= RegOpenKeyEx(HKEY_LOCAL_MACHINE, 'Software\Microsoft\DirectX SDK', 0, KEY_READ, Key);
  if (ERROR_SUCCESS <> lResult) then Exit;

  dwSize:= cchDest * SizeOf(Char);
  lResult:= RegQueryValueEx(Key, 'DX9SDK Samples Path', nil,
                            @dwType, PByte(strDest), @dwSize);
  strDest[cchDest-1]:= #0; // RegQueryValueEx doesn't NULL term if buffer too small
  RegCloseKey(Key);

  if(ERROR_SUCCESS <> lResult) then Exit;

  if (StrLen(strDest) + StrLen(strMedia) < Cardinal(cchDest)) then
    StrCat(strDest, strMedia)
  else begin
    Result:= E_INVALIDARG;
    Exit;
  end;

  Result:= S_OK;
end;
{$ENDIF} // !UNDER_CE



{$IFNDEF UNDER_CE}
//-----------------------------------------------------------------------------
// Name: DXUtil_FindMediaFileCch()
// Desc: Returns a valid path to a DXSDK media file
//       cchDest is the size in TCHARs of strDestPath.  Be careful not to
//       pass in sizeof(strDest) on UNICODE builds.
//-----------------------------------------------------------------------------
function DXUtil_FindMediaFileCch(strDestPath: PChar; cchDest: Integer; strFilename: PChar): HRESULT;
var
  file_: THandle;
  strShortNameTmp: PChar;
  strShortName: array[0..MAX_PATH-1] of Char;
  cchPath: Integer;
begin
  strShortNameTmp:= nil;

  if (nil = strFilename) or (nil = strDestPath) or (cchDest < 1) then
  begin
    Result:= E_INVALIDARG;
    Exit;
  end;

  StrCopy(strDestPath, '');
  StrCopy(strShortName, '');

  // Build full path name from strFileName (strShortName will be just the leaf filename)
  cchPath:= GetFullPathName(strFilename, cchDest, strDestPath, strShortNameTmp);
  if ((cchPath = 0) or (cchDest <= cchPath)) then
  begin
    Result:= E_FAIL;
    Exit;
  end;
  if (strShortNameTmp <> nil) then
    StrLCopy(strShortName, strShortNameTmp, MAX_PATH);

  // first try to find the filename given a full path
  file_:= CreateFile(strDestPath, GENERIC_READ, FILE_SHARE_READ, nil,
                     OPEN_EXISTING, 0, 0);
  if (INVALID_HANDLE_VALUE <> file_) then
  begin
    CloseHandle(file_);
    Result:= S_OK;
    Exit;
  end;

  // next try to find the filename in the current working directory (path stripped)
  file_:= CreateFile(strShortName, GENERIC_READ, FILE_SHARE_READ, nil,
                     OPEN_EXISTING, 0, 0);
  if (INVALID_HANDLE_VALUE <> file_) then
  begin
    StrLCopy(strDestPath, strShortName, cchDest);
    strDestPath[cchDest-1]:= #0; // StrLCopy(? - _tcsncpy) doesn't NULL term if it runs out of space
    CloseHandle(file_);
    Result:= S_OK;
    Exit;
  end;

  // last, check if the file exists in the media directory
  Result:= DXUtil_GetDXSDKMediaPathCch(strDestPath, cchDest);
  if FAILED(Result) then Exit;

  if (lstrlen(strDestPath) + lstrlen(strShortName) < cchDest) then
    StrCat(strDestPath, strShortName)
  else begin
    Result:= E_INVALIDARG;
    Exit;
  end;

  file_:= CreateFile(strDestPath, GENERIC_READ, FILE_SHARE_READ, nil,
                     OPEN_EXISTING, 0, 0);
  if (INVALID_HANDLE_VALUE <> file_) then
  begin
    CloseHandle(file_);
    Result:= S_OK;
    Exit;
  end;

  // On failure, just return the file as the path
  StrLCopy(strDestPath, strFilename, cchDest);
  strDestPath[cchDest-1]:= #0; // StrLCopy(? - _tcsncpy) doesn't NULL term if it runs out of space
  Result:= E_FAIL; // HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND); //todo: ERROR_FILE_NOT_FOUND
end;
{$ENDIF} // !UNDER_CE




//-----------------------------------------------------------------------------
// Name: DXUtil_ReadStringRegKeyCch()
// Desc: Helper function to read a registry key string
//       cchDest is the size in TCHARs of strDest.  Be careful not to
//       pass in sizeof(strDest) on UNICODE builds.
//-----------------------------------------------------------------------------
function DXUtil_ReadStringRegKeyCch(hKey_: HKEY; strRegName: PChar; strDest: PChar; cchDest: DWORD; strDefault: PChar): HRESULT;
var
  dwType: DWORD;
  cbDest: DWORD;
begin
  cbDest:= cchDest*SizeOf(Char);

  if (ERROR_SUCCESS <> RegQueryValueEx(hKey_, strRegName, nil, @dwType,
                                       PByte(strDest), @cbDest)) then
  begin
    StrLCopy(strDest, strDefault, cchDest);
    strDest[cchDest-1]:= #0;

    if (dwType <> REG_SZ) then
    begin
      Result:= E_FAIL;
      Exit;
    end;

    Result:= S_OK;
    Exit;
  end;

  Result:= E_FAIL;
end;




//-----------------------------------------------------------------------------
// Name: DXUtil_WriteStringRegKey()
// Desc: Helper function to write a registry key string
//-----------------------------------------------------------------------------
function DXUtil_WriteStringRegKey(hKey_: HKEY; strRegName: PChar; strValue: PChar): HRESULT;
var
  cbValue: DWORD;
begin
  if (nil = strValue) then
  begin
    Result:= E_INVALIDARG;
    Exit;
  end;

  cbValue:= (StrLen(strValue)+1) * SizeOf(Char);

  if (ERROR_SUCCESS <> RegSetValueEx(hKey_, strRegName, 0, REG_SZ,
                                     PByte(strValue), cbValue)) then
  begin
    Result:= E_FAIL;
    Exit;
  end;

  Result:= S_OK;
end;



(*
//-----------------------------------------------------------------------------
// Name: DXUtil_ReadIntRegKey()
// Desc: Helper function to read a registry key int
//-----------------------------------------------------------------------------
HRESULT DXUtil_ReadIntRegKey( HKEY hKey, TCHAR* strRegName, DWORD* pdwDest, 
                              DWORD dwDefault )
{
    DWORD dwType;
    DWORD dwLength = sizeof(DWORD);

    if( ERROR_SUCCESS != RegQueryValueEx( hKey, strRegName, 0, &dwType, 
                                          (BYTE* )pdwDest, &dwLength ) )
    {
        *pdwDest = dwDefault;
        if( dwType != REG_DWORD )
            return E_FAIL;

        return S_OK;
    }

    return E_FAIL;
}




//-----------------------------------------------------------------------------
// Name: DXUtil_WriteIntRegKey()
// Desc: Helper function to write a registry key int
//-----------------------------------------------------------------------------
HRESULT DXUtil_WriteIntRegKey( HKEY hKey, TCHAR* strRegName, DWORD dwValue )
{
    if( ERROR_SUCCESS != RegSetValueEx( hKey, strRegName, 0, REG_DWORD, 
                                        (BYTE* )&dwValue, sizeof(DWORD) ) )
        return E_FAIL;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DXUtil_ReadBoolRegKey()
// Desc: Helper function to read a registry key BOOL
//-----------------------------------------------------------------------------
HRESULT DXUtil_ReadBoolRegKey( HKEY hKey, TCHAR* strRegName, BOOL* pbDest, 
                              BOOL bDefault )
{
    DWORD dwType;
    DWORD dwLength = sizeof(BOOL);

    if( ERROR_SUCCESS != RegQueryValueEx( hKey, strRegName, 0, &dwType, 
                                          (BYTE* )pbDest, &dwLength ) )
    {
        *pbDest = bDefault;
        if( dwType != REG_DWORD )
            return E_FAIL;

        return S_OK;
    }

    return E_FAIL;
}




//-----------------------------------------------------------------------------
// Name: DXUtil_WriteBoolRegKey()
// Desc: Helper function to write a registry key BOOL
//-----------------------------------------------------------------------------
HRESULT DXUtil_WriteBoolRegKey( HKEY hKey, TCHAR* strRegName, BOOL bValue )
{
    if( ERROR_SUCCESS != RegSetValueEx( hKey, strRegName, 0, REG_DWORD, 
                                        (BYTE* )&bValue, sizeof(BOOL) ) )
        return E_FAIL;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DXUtil_ReadGuidRegKey()
// Desc: Helper function to read a registry key guid
//-----------------------------------------------------------------------------
HRESULT DXUtil_ReadGuidRegKey( HKEY hKey, TCHAR* strRegName, GUID* pGuidDest, 
                               GUID& guidDefault )
{
    DWORD dwType;
    DWORD dwLength = sizeof(GUID);

    if( ERROR_SUCCESS != RegQueryValueEx( hKey, strRegName, 0, &dwType, 
                                          (LPBYTE) pGuidDest, &dwLength ) )
    {
        *pGuidDest = guidDefault;
        if( dwType != REG_BINARY )
            return E_FAIL;

        return S_OK;
    }

    return E_FAIL;
}




//-----------------------------------------------------------------------------
// Name: DXUtil_WriteGuidRegKey()
// Desc: Helper function to write a registry key guid
//-----------------------------------------------------------------------------
HRESULT DXUtil_WriteGuidRegKey( HKEY hKey, TCHAR* strRegName, GUID guidValue )
{
    if( ERROR_SUCCESS != RegSetValueEx( hKey, strRegName, 0, REG_BINARY, 
                                        (BYTE* )&guidValue, sizeof(GUID) ) )
        return E_FAIL;

    return S_OK;
}*)




//-----------------------------------------------------------------------------
// Name: DXUtil_Timer()
// Desc: Performs timer opertations. Use the following commands:
//          TIMER_RESET           - to reset the timer
//          TIMER_START           - to start the timer
//          TIMER_STOP            - to stop (or pause) the timer
//          TIMER_ADVANCE         - to advance the timer by 0.1 seconds
//          TIMER_GETABSOLUTETIME - to get the absolute system time
//          TIMER_GETAPPTIME      - to get the current time
//          TIMER_GETELAPSEDTIME  - to get the time that elapsed between 
//                                  TIMER_GETELAPSEDTIME calls
//-----------------------------------------------------------------------------
function DXUtil_Timer(command: TIMER_COMMAND): Single; stdcall;
{$WRITEABLECONST ON}
const
  m_bTimerInitialized: BOOL   = FALSE;
  m_bUsingQPF: BOOL           = FALSE;
  m_bTimerStopped: BOOL       = TRUE;
  m_llQPFTicksPerSec: Int64   = 0;
  m_llStopTime: Int64         = 0;
  m_llLastElapsedTime: Int64  = 0;
  m_llBaseTime: Int64         = 0;
  m_fLastElapsedTime: Double  = 0.0;
  m_fBaseTime: Double         = 0.0;
  m_fStopTime: Double         = 0.0;
{$WRITEABLECONST OFF}
var
  qwTicksPerSec: Int64; // LARGE_INTEGER;
  qwTime: Int64; // LARGE_INTEGER;
  fTime: Double;
  fElapsedTime: Double;
  fAppTime: Double;
begin
  // Initialize the timer
  if (FALSE = m_bTimerInitialized) then
  begin
    m_bTimerInitialized:= True;

    // Use QueryPerformanceFrequency() to get frequency of timer.  If QPF is
    // not supported, we will timeGetTime() which returns milliseconds.

    m_bUsingQPF:= QueryPerformanceFrequency(qwTicksPerSec);
    if (m_bUsingQPF) then
      m_llQPFTicksPerSec:= qwTicksPerSec{.QuadPart};
  end;

  if (m_bUsingQPF) then
  begin
    // Get either the current time or the stop time, depending
    // on whether we're stopped and what command was sent
    if (m_llStopTime <> 0) and (command <> TIMER_START) and (command <> TIMER_GETABSOLUTETIME) then
      qwTime{.QuadPart}:= m_llStopTime
    else
      QueryPerformanceCounter(Int64(qwTime));

    // Return the elapsed time
    if (command = TIMER_GETELAPSEDTIME) then
    begin
      fElapsedTime:= (qwTime{.QuadPart} - m_llLastElapsedTime) /  m_llQPFTicksPerSec;
      m_llLastElapsedTime:= qwTime{.QuadPart};
      Result:= fElapsedTime;
      Exit;
    end;

    // Return the current time
    if (command = TIMER_GETAPPTIME) then
    begin
      fAppTime:= (qwTime{.QuadPart} - m_llBaseTime) /  m_llQPFTicksPerSec;
      Result:= fAppTime;
      Exit;
    end;

    // Reset the timer
    if (command = TIMER_RESET) then
    begin
      m_llBaseTime        := qwTime{.QuadPart};
      m_llLastElapsedTime := qwTime{.QuadPart};
      m_llStopTime        := 0;
      m_bTimerStopped     := False;
      Result:= 0.0;
      Exit;
    end;

    // Start the timer
    if (command = TIMER_START) then
    begin
      if m_bTimerStopped then
        m_llBaseTime:= m_llBaseTime + (qwTime{.QuadPart} - m_llStopTime);
      m_bTimerStopped:= False;
      m_llStopTime:= 0;
      m_llLastElapsedTime:= qwTime{.QuadPart};
      Result:=0.0;
      Exit;
    end;

    // Stop the timer
    if (command = TIMER_STOP) then
    begin
      if not m_bTimerStopped then
      begin
        m_llStopTime:= qwTime{.QuadPart};
        m_llLastElapsedTime:= qwTime{.QuadPart};
        m_bTimerStopped:= True;
      end;
      Result:= 0.0;
      Exit;
    end;

    // Advance the timer by 1/10th second
    if (command = TIMER_ADVANCE) then
    begin
      m_llStopTime:= Trunc(m_llStopTime + m_llQPFTicksPerSec/10);
      Result:= 0.0;
      Exit;
    end;

    if (command = TIMER_GETABSOLUTETIME) then
    begin
      fTime:= qwTime{.QuadPart} / m_llQPFTicksPerSec;
      Result:= fTime;
      Exit;
    end;

    Result:= -1.0; // Invalid command specified
    Exit;
  end else
  begin
    // Get the time using timeGetTime()

    // Get either the current time or the stop time, depending
    // on whether we're stopped and what command was sent
    if (m_fStopTime <> 0.0) and (command <> TIMER_START) and (command <> TIMER_GETABSOLUTETIME) then
      fTime:= m_fStopTime
    else
      fTime:= GETTIMESTAMP * 0.001;

    // Return the elapsed time
    if (command = TIMER_GETELAPSEDTIME) then
    begin
      fElapsedTime:= (fTime - m_fLastElapsedTime);
      m_fLastElapsedTime:= fTime;
      Result:= fElapsedTime;
      Exit;
    end;

    // Return the current time
    if (command = TIMER_GETAPPTIME) then
    begin
      Result:= (fTime - m_fBaseTime);
      Exit;
    end;

    // Reset the timer
    if (command = TIMER_RESET) then
    begin
      m_fBaseTime         := fTime;
      m_fLastElapsedTime  := fTime;
      m_fStopTime         := 0;
      m_bTimerStopped     := False;
      Result:= 0.0;
      Exit;
    end;

    // Start the timer
    if (command = TIMER_START) then
    begin
      if m_bTimerStopped then
        m_fBaseTime:= m_fBaseTime + (fTime - m_fStopTime);
      m_bTimerStopped:= False;
      m_fStopTime:= 0.0;
      m_fLastElapsedTime:= fTime;
      Result:= 0.0;
      Exit;
    end;

    // Stop the timer
    if (command = TIMER_STOP) then
    begin
      if not m_bTimerStopped then
      begin
        m_fStopTime:= fTime;
        m_fLastElapsedTime:= fTime;
        m_bTimerStopped:= True;
      end;
      Result:= 0.0;
      Exit;
    end;

    // Advance the timer by 1/10th second
    if (command = TIMER_ADVANCE) then
    begin
      m_fStopTime:= m_fStopTime + 0.1;
      Result:= 0.0;
      Exit;
    end;

    if (command = TIMER_GETABSOLUTETIME) then
    begin
      Result:= fTime;
      Exit;
    end;

    Result:= -1.0; // Invalid command specified
  end;
end;



(*
//-----------------------------------------------------------------------------
// Name: DXUtil_ConvertAnsiStringToWideCch()
// Desc: This is a UNICODE conversion utility to convert a CHAR string into a
//       WCHAR string. 
//       cchDestChar is the size in TCHARs of wstrDestination.  Be careful not to 
//       pass in sizeof(strDest) 
//-----------------------------------------------------------------------------
HRESULT DXUtil_ConvertAnsiStringToWideCch( WCHAR* wstrDestination, const CHAR* strSource, 
                                     int cchDestChar )
{
    if( wstrDestination==NULL || strSource==NULL || cchDestChar < 1 )
        return E_INVALIDARG;

    int nResult = MultiByteToWideChar( CP_ACP, 0, strSource, -1, 
                                       wstrDestination, cchDestChar );
    wstrDestination[cchDestChar-1] = 0;
    
    if( nResult == 0 )
        return E_FAIL;
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DXUtil_ConvertWideStringToAnsi()
// Desc: This is a UNICODE conversion utility to convert a WCHAR string into a
//       CHAR string.
//       cchDestChar is the size in TCHARs of strDestination
//-----------------------------------------------------------------------------
HRESULT DXUtil_ConvertWideStringToAnsiCch( CHAR* strDestination, const WCHAR* wstrSource, 
                                     int cchDestChar )
{
    if( strDestination==NULL || wstrSource==NULL || cchDestChar < 1 )
        return E_INVALIDARG;

    int nResult = WideCharToMultiByte( CP_ACP, 0, wstrSource, -1, strDestination, 
                                       cchDestChar*sizeof(CHAR), NULL, NULL );
    strDestination[cchDestChar-1] = 0;
    
    if( nResult == 0 )
        return E_FAIL;
    return S_OK;
} *)




//-----------------------------------------------------------------------------
// Name: DXUtil_ConvertGenericStringToAnsi()
// Desc: This is a UNICODE conversion utility to convert a TCHAR string into a
//       CHAR string.
//       cchDestChar is the size in TCHARs of strDestination
//-----------------------------------------------------------------------------
function DXUtil_ConvertGenericStringToAnsiCch(strDestination: PAnsiChar; const tstrSource: PChar; cchDestChar: Integer = -1): HRESULT;
begin
  if (strDestination = nil) or (tstrSource = nil) or (cchDestChar < 1) then
  begin
    Result:= E_INVALIDARG;
    Exit;
  end;

{$IFDEF UNICODE}
  Result:= DXUtil_ConvertWideStringToAnsiCch(strDestination, tstrSource, cchDestChar);
{$ELSE}
  StrLCopy(strDestination, tstrSource, cchDestChar);
  strDestination[cchDestChar-1] := #0;
  Result:= S_OK;
{$ENDIF}
end;



(*
//-----------------------------------------------------------------------------
// Name: DXUtil_ConvertGenericStringToWide()
// Desc: This is a UNICODE conversion utility to convert a TCHAR string into a
//       WCHAR string.
//       cchDestChar is the size in TCHARs of wstrDestination.  Be careful not to
//       pass in sizeof(strDest)
//-----------------------------------------------------------------------------
HRESULT DXUtil_ConvertGenericStringToWideCch( WCHAR* wstrDestination, const TCHAR* tstrSource,
                                           int cchDestChar )
{
    if( wstrDestination==NULL || tstrSource==NULL || cchDestChar < 1 )
        return E_INVALIDARG;

#ifdef _UNICODE
    wcsncpy( wstrDestination, tstrSource, cchDestChar );
    wstrDestination[cchDestChar-1] = L'\0';
    return S_OK;
#else
    return DXUtil_ConvertAnsiStringToWideCch( wstrDestination, tstrSource, cchDestChar );
#endif    
} *)




//-----------------------------------------------------------------------------
// Name: DXUtil_ConvertAnsiStringToGeneric()
// Desc: This is a UNICODE conversion utility to convert a CHAR string into a
//       TCHAR string.
//       cchDestChar is the size in TCHARs of tstrDestination.  Be careful not to
//       pass in sizeof(strDest) on UNICODE builds
//-----------------------------------------------------------------------------
function DXUtil_ConvertAnsiStringToGenericCch(tstrDestination: PChar;
  const strSource: PAnsiChar; cchDestChar: Integer = -1): HRESULT;
begin
  if (tstrDestination = nil) or (strSource = nil) or (cchDestChar < 1) then
  begin
    Result:= E_INVALIDARG;
    Exit;
  end;

{$IFDEF UNICODE}
  return DXUtil_ConvertAnsiStringToWideCch( tstrDestination, strSource, cchDestChar );
{$ELSE}
  StrLCopy(tstrDestination, strSource, cchDestChar);
  tstrDestination[cchDestChar-1] := #0;
  Result:= S_OK;
{$ENDIF}
end;




(*
//-----------------------------------------------------------------------------
// Name: DXUtil_ConvertAnsiStringToGeneric()
// Desc: This is a UNICODE conversion utility to convert a WCHAR string into a
//       TCHAR string. 
//       cchDestChar is the size in TCHARs of tstrDestination.  Be careful not to 
//       pass in sizeof(strDest) on UNICODE builds
//-----------------------------------------------------------------------------
HRESULT DXUtil_ConvertWideStringToGenericCch( TCHAR* tstrDestination, const WCHAR* wstrSource, 
                                           int cchDestChar )
{
    if( tstrDestination==NULL || wstrSource==NULL || cchDestChar < 1 )
        return E_INVALIDARG;

#ifdef _UNICODE
    wcsncpy( tstrDestination, wstrSource, cchDestChar );
    tstrDestination[cchDestChar-1] = L'\0';    
    return S_OK;
#else
    return DXUtil_ConvertWideStringToAnsiCch( tstrDestination, wstrSource, cchDestChar );
#endif
}

*)(*



//-----------------------------------------------------------------------------
// Name: DXUtil_LaunchReadme()
// Desc: Finds and opens the readme.txt for this sample
//-----------------------------------------------------------------------------
procedure DXUtil_LaunchReadme(hWnd: THandle; strLoc: PChar{$IFDEF SUPPORTS_DEFAULTPARAMS} = nil{$ENDIF});
{$IFDEF UNDER_CE}
    // This is not available on PocketPC
    MessageBox( hWnd, TEXT("For operating instructions, please open the ")
                      TEXT("readme.txt file included with the project."),
                TEXT("DirectX SDK Sample"), MB_ICONWARNING | MB_OK );

    return;
{$ELSE}

begin
    bool bSuccess = false;
    bool bFound = false;
    TCHAR strReadmePath[1024];
    TCHAR strExeName[MAX_PATH];
    TCHAR strExePath[MAX_PATH];
    TCHAR strSamplePath[MAX_PATH];
    TCHAR* strLastSlash = NULL;

    lstrcpy( strReadmePath, TEXT("") );
    lstrcpy( strExePath, TEXT("") );
    lstrcpy( strExeName, TEXT("") );
    lstrcpy( strSamplePath, TEXT("") );

    // If the user provided a location for the readme, check there first.
    if( strLoc )
    {
        HKEY  hKey;
        LONG lResult = RegOpenKeyEx( HKEY_LOCAL_MACHINE,
                                    _T("Software\\Microsoft\\DirectX SDK"),
                                    0, KEY_READ, &hKey );
        if( ERROR_SUCCESS == lResult )
        {
            DWORD dwType;
            DWORD dwSize = MAX_PATH * sizeof(TCHAR);
            lResult = RegQueryValueEx( hKey, _T("DX9SDK Samples Path"), NULL,
                                      &dwType, (BYTE* )strSamplePath, &dwSize );
            strSamplePath[MAX_PATH-1] = 0; // RegQueryValueEx doesn't NULL term if buffer too small
            
            if( ERROR_SUCCESS == lResult )
            {
                _sntprintf( strReadmePath, 1023, TEXT("%s\\C++\\%s\\readme.txt"), 
                            strSamplePath, strLoc );
                strReadmePath[1023] = 0;

                if( GetFileAttributes( strReadmePath ) != 0xFFFFFFFF )
                    bFound = TRUE;
            }
        }

        RegCloseKey( hKey );
    }

    // Get the exe name, and exe path
    GetModuleFileName( NULL, strExePath, MAX_PATH );
    strExePath[MAX_PATH-1]=0;

    strLastSlash = _tcsrchr( strExePath, TEXT('\\') );
    if( strLastSlash )
    {
        _tcsncpy( strExeName, &strLastSlash[1], MAX_PATH );
        strExeName[MAX_PATH-1]=0;

        // Chop the exe name from the exe path
        *strLastSlash = 0;

        // Chop the .exe from the exe name
        strLastSlash = _tcsrchr( strExeName, TEXT('.') );
        if( strLastSlash )
            *strLastSlash = 0;
    }

    if( !bFound )
    {
        // Search in "%EXE_DIR%\..\%EXE_NAME%".  This matchs the DirectX SDK layout
        _tcscpy( strReadmePath, strExePath );

        strLastSlash = _tcsrchr( strReadmePath, TEXT('\\') );
        if( strLastSlash )
            *strLastSlash = 0;
        lstrcat( strReadmePath, TEXT("\\") );
        lstrcat( strReadmePath, strExeName );
        lstrcat( strReadmePath, TEXT("\\readme.txt") );
        if( GetFileAttributes( strReadmePath ) != 0xFFFFFFFF )
            bFound = TRUE;
    }

    if( !bFound )
    {
        // Search in "%EXE_DIR%\"
        _tcscpy( strReadmePath, strExePath );
        lstrcat( strReadmePath, TEXT("\\readme.txt") );
        if( GetFileAttributes( strReadmePath ) != 0xFFFFFFFF )
            bFound = TRUE;
    }

    if( !bFound )
    {
        // Search in "%EXE_DIR%\.."
        _tcscpy( strReadmePath, strExePath );
        strLastSlash = _tcsrchr( strReadmePath, TEXT('\\') );
        if( strLastSlash )
            *strLastSlash = 0;
        lstrcat( strReadmePath, TEXT("\\readme.txt") );
        if( GetFileAttributes( strReadmePath ) != 0xFFFFFFFF )
            bFound = TRUE;
    }

    if( !bFound )
    {
        // Search in "%EXE_DIR%\..\.."
        _tcscpy( strReadmePath, strExePath );
        strLastSlash = _tcsrchr( strReadmePath, TEXT('\\') );
        if( strLastSlash )
            *strLastSlash = 0;
        strLastSlash = _tcsrchr( strReadmePath, TEXT('\\') );
        if( strLastSlash )
            *strLastSlash = 0;
        lstrcat( strReadmePath, TEXT("\\readme.txt") );
        if( GetFileAttributes( strReadmePath ) != 0xFFFFFFFF )
            bFound = TRUE;
    }

    if( bFound )
    {
        // GetProcAddress for ShellExecute, so we don't have to include shell32.lib 
        // in every project that uses dxutil.cpp
        LPShellExecute pShellExecute = NULL;
        HINSTANCE hInstShell32 = LoadLibrary(TEXT("shell32.dll"));
        if (hInstShell32 != NULL)
        {
#ifdef UNICODE
            pShellExecute = (LPShellExecute)GetProcAddress(hInstShell32, _TWINCE("ShellExecuteW"));
#else
            pShellExecute = (LPShellExecute)GetProcAddress(hInstShell32, _TWINCE("ShellExecuteA"));
#endif
            if( pShellExecute != NULL )
            {
                if( pShellExecute( hWnd, TEXT("open"), strReadmePath, NULL, NULL, SW_SHOW ) > (HINSTANCE) 32 )
                    bSuccess = true;
            }

            FreeLibrary(hInstShell32);
        }
    }

    if( !bSuccess )
    {
        // Tell the user that the readme couldn't be opened
        MessageBox( hWnd, TEXT("Could not find readme.txt"), 
                    TEXT("DirectX SDK Sample"), MB_ICONWARNING | MB_OK );
    }

#endif // UNDER_CE
}





//-----------------------------------------------------------------------------
// Name: DXUtil_Trace()
// Desc: Outputs to the debug stream a formatted string with a variable-
//       argument list.
//-----------------------------------------------------------------------------
VOID DXUtil_Trace( TCHAR* strMsg, ... )
{
#if defined(DEBUG) | defined(_DEBUG)
    TCHAR strBuffer[512];
    
    va_list args;
    va_start(args, strMsg);
    _vsntprintf( strBuffer, 512, strMsg, args );
    va_end(args);

    OutputDebugString( strBuffer );
#else
    UNREFERENCED_PARAMETER(strMsg);
#endif
}




//-----------------------------------------------------------------------------
// Name: DXUtil_ConvertStringToGUID()
// Desc: Converts a string to a GUID
//-----------------------------------------------------------------------------
HRESULT DXUtil_ConvertStringToGUID( const TCHAR* strSrc, GUID* pGuidDest )
{
    UINT aiTmp[10];

    if( _stscanf( strSrc, TEXT("{%8X-%4X-%4X-%2X%2X-%2X%2X%2X%2X%2X%2X}"),
                    &pGuidDest->Data1, 
                    &aiTmp[0], &aiTmp[1], 
                    &aiTmp[2], &aiTmp[3],
                    &aiTmp[4], &aiTmp[5],
                    &aiTmp[6], &aiTmp[7],
                    &aiTmp[8], &aiTmp[9] ) != 11 )
    {
        ZeroMemory( pGuidDest, sizeof(GUID) );
        return E_FAIL;
    }
    else
    {
        pGuidDest->Data2       = (USHORT) aiTmp[0];
        pGuidDest->Data3       = (USHORT) aiTmp[1];
        pGuidDest->Data4[0]    = (BYTE) aiTmp[2];
        pGuidDest->Data4[1]    = (BYTE) aiTmp[3];
        pGuidDest->Data4[2]    = (BYTE) aiTmp[4];
        pGuidDest->Data4[3]    = (BYTE) aiTmp[5];
        pGuidDest->Data4[4]    = (BYTE) aiTmp[6];
        pGuidDest->Data4[5]    = (BYTE) aiTmp[7];
        pGuidDest->Data4[6]    = (BYTE) aiTmp[8];
        pGuidDest->Data4[7]    = (BYTE) aiTmp[9];
        return S_OK;
    }
}




//-----------------------------------------------------------------------------
// Name: DXUtil_ConvertGUIDToStringCch()
// Desc: Converts a GUID to a string 
//       cchDestChar is the size in TCHARs of strDest.  Be careful not to 
//       pass in sizeof(strDest) on UNICODE builds
//-----------------------------------------------------------------------------
HRESULT DXUtil_ConvertGUIDToStringCch( const GUID* pGuidSrc, TCHAR* strDest, int cchDestChar )
{
    int nResult = _sntprintf( strDest, cchDestChar, TEXT("{%0.8X-%0.4X-%0.4X-%0.2X%0.2X-%0.2X%0.2X%0.2X%0.2X%0.2X%0.2X}"),
               pGuidSrc->Data1, pGuidSrc->Data2, pGuidSrc->Data3,
               pGuidSrc->Data4[0], pGuidSrc->Data4[1],
               pGuidSrc->Data4[2], pGuidSrc->Data4[3],
               pGuidSrc->Data4[4], pGuidSrc->Data4[5],
               pGuidSrc->Data4[6], pGuidSrc->Data4[7] );

    if( nResult < 0 )
        return E_FAIL;
    return S_OK;
}
*)




{ CArrayList }

//-----------------------------------------------------------------------------
// Name: CArrayList constructor
// Desc: 
//-----------------------------------------------------------------------------
constructor CArrayList.Create(_Type: TArrayListType;
  BytesPerEntry: Integer = 0);
begin
  if (_Type = AL_REFERENCE) then
    BytesPerEntry:= SizeOf(Pointer);
  m_ArrayListType:= _Type;
  m_pData:= nil;
  m_BytesPerEntry:= BytesPerEntry;
  m_NumEntries:= 0;
  m_NumEntriesAllocated:= 0;
end;

//-----------------------------------------------------------------------------
// Name: CArrayList destructor
// Desc: 
//-----------------------------------------------------------------------------
destructor CArrayList.Destroy;
begin
  if (m_pData <> nil) then FreeMem(m_pData);
  inherited;
end;

//-----------------------------------------------------------------------------
// Name: CArrayList::Add
// Desc: Adds pEntry to the list.
//-----------------------------------------------------------------------------
function CArrayList.Add(pEntry: Pointer): HRESULT;
var
  pDataNew: Pointer;
  NumEntriesAllocatedNew: Integer;
begin
  if (m_BytesPerEntry = 0) then
  begin
    Result:= E_FAIL;
    Exit;
  end;

  if (m_pData = nil) or (m_NumEntries + 1 > m_NumEntriesAllocated) then
  begin
    if (m_NumEntriesAllocated = 0) then
      NumEntriesAllocatedNew:= 16
    else
      NumEntriesAllocatedNew:= m_NumEntriesAllocated * 2;

    try
      GetMem(pDataNew, NumEntriesAllocatedNew * m_BytesPerEntry);
    except
      Result:= E_OUTOFMEMORY;
      Exit;
    end;

    if (m_pData <> nil) then
    begin
      CopyMemory(pDataNew, m_pData, m_NumEntries * m_BytesPerEntry);
      FreeMem(m_pData)
    end;
    m_pData:= pDataNew;
    m_NumEntriesAllocated:= NumEntriesAllocatedNew;
  end;

  if (m_ArrayListType = AL_VALUE) then
    CopyMemory(Pointer(DWORD(m_pData) + DWORD(m_NumEntries * m_BytesPerEntry)), pEntry, m_BytesPerEntry)
  else
    PPointer(DWORD(m_pData) + DWORD(m_NumEntries)*SizeOf(Pointer))^:= pEntry;
  Inc(m_NumEntries);

  Result:= S_OK;
end;

//-----------------------------------------------------------------------------
// Name: CArrayList::Remove
// Desc: Remove the item at Entry in the list, and collapse the array.
//-----------------------------------------------------------------------------
procedure CArrayList.Remove(Entry: Integer);
var
  pData: Pointer;
begin
  // Decrement count
  Dec(m_NumEntries);

  // Find the entry address
  pData:= Pointer(DWORD(m_pData) + DWORD(Entry * m_BytesPerEntry));

  // Collapse the array
  MoveMemory(pData, Pointer(DWORD(pData) + DWORD(m_BytesPerEntry)), (m_NumEntries - Entry)*m_BytesPerEntry);
end;

//-----------------------------------------------------------------------------
// Name: CArrayList::GetPtr
// Desc: Returns a pointer to the Entry'th entry in the list.
//-----------------------------------------------------------------------------
function CArrayList.GetPtr(Entry: Integer): Pointer;
begin
  if (m_ArrayListType = AL_VALUE) then
    Result:= Pointer(DWORD(m_pData) + DWORD(Entry * m_BytesPerEntry))
  else
    Result:= PPointer(DWORD(m_pData) + DWORD(Entry)*SizeOf(Pointer))^;
end;

//-----------------------------------------------------------------------------
// Name: CArrayList::Contains
// Desc: Returns whether the list contains an entry identical to the
//       specified entry data.
//-----------------------------------------------------------------------------
function CArrayList.Contains(pEntryData: Pointer): Boolean;
var
  iEntry: LongWord;
begin
  Result:= False;
  if (m_NumEntries < 1) then Exit;

  for iEntry:= 0 to m_NumEntries - 1 do
  begin
    if (m_ArrayListType = AL_VALUE) then
    begin
      if CompareMem(GetPtr(iEntry), pEntryData, m_BytesPerEntry) then
      begin
        Result:= True;
        Exit;
      end;
    end else
    begin
      if (GetPtr(iEntry) = pEntryData) then
      begin
        Result:= True;
        Exit;
      end;
    end;
  end;
end;

procedure CArrayList.Clear;
begin
  m_NumEntries:= 0;
end;



(*
//-----------------------------------------------------------------------------
// Name: BYTE helper functions
// Desc: cchDestChar is the size in BYTEs of strDest.  Be careful not to
//       pass use sizeof() if the strDest is a string pointer.
//       eg.
//       TCHAR* sz = new TCHAR[100]; // sizeof(sz)  == 4
//       TCHAR sz2[100];             // sizeof(sz2) == 200
//-----------------------------------------------------------------------------
HRESULT DXUtil_ConvertAnsiStringToWideCb( WCHAR* wstrDestination, const CHAR* strSource, int cbDestChar )
{
    return DXUtil_ConvertAnsiStringToWideCch( wstrDestination, strSource, cbDestChar / sizeof(WCHAR) );
}

HRESULT DXUtil_ConvertWideStringToAnsiCb( CHAR* strDestination, const WCHAR* wstrSource, int cbDestChar )
{
    return DXUtil_ConvertWideStringToAnsiCch( strDestination, wstrSource, cbDestChar / sizeof(CHAR) );
} *)

function DXUtil_ConvertGenericStringToAnsiCb(strDestination: PAnsiChar; const tstrSource: PChar; cbDestChar: Integer): HRESULT;
begin
  Result:= DXUtil_ConvertGenericStringToAnsiCch(strDestination, tstrSource, cbDestChar div SizeOf(AnsiChar));
end;

(*
HRESULT DXUtil_ConvertGenericStringToWideCb( WCHAR* wstrDestination, const TCHAR* tstrSource, int cbDestChar )
{
    return DXUtil_ConvertGenericStringToWideCch( wstrDestination, tstrSource, cbDestChar / sizeof(WCHAR) );
} *)

function DXUtil_ConvertAnsiStringToGenericCb(tstrDestination: PChar; const strSource: PChar; cbDestChar: Integer): HRESULT;
begin
  Result:= DXUtil_ConvertAnsiStringToGenericCch(tstrDestination, strSource, cbDestChar div SizeOf(Char));
end;

(* HRESULT DXUtil_ConvertWideStringToGenericCb( TCHAR* tstrDestination, const WCHAR* wstrSource, int cbDestChar )
{
    return DXUtil_ConvertWideStringToGenericCch( tstrDestination, wstrSource, cbDestChar / sizeof(TCHAR) );
}

HRESULT DXUtil_ReadStringRegKeyCb( HKEY hKey, TCHAR* strRegName, TCHAR* strDest, DWORD cbDest, TCHAR* strDefault )
{
    return DXUtil_ReadStringRegKeyCch( hKey, strRegName, strDest, cbDest / sizeof(TCHAR), strDefault );
}

HRESULT DXUtil_ConvertGUIDToStringCb( const GUID* pGuidSrc, TCHAR* strDest, int cbDestChar )
{
    return DXUtil_ConvertGUIDToStringCch( pGuidSrc, strDest, cbDestChar / sizeof(TCHAR) );
}*)

{$IFNDEF UNDER_CE}
function DXUtil_GetDXSDKMediaPathCb(szDest: PChar; cbDest: Integer): HRESULT;
begin
  Result:= DXUtil_GetDXSDKMediaPathCch(szDest, cbDest div SizeOf(Char));
end;

function DXUtil_FindMediaFileCb(szDestPath: PChar; cbDest: Integer; strFilename: PChar): HRESULT;
begin
  Result:= DXUtil_FindMediaFileCch(szDestPath, cbDest div SizeOf(Char), strFilename);
end;
{$ENDIF} // !UNDER_CE


//#define GETTIMESTAMP GetTickCount
function GETTIMESTAMP: DWORD;
begin
  Result:= GetTickCount;
end;

end.

