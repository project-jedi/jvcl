{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSHFmt.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{ A component that shows the "format floppy" dialog in windows }
unit JvSHFmt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, JVCLVer;

type
  TJvFormatType = (ftQuick, ftStandard, ftBootable);
  TJvDriveLetters = 'A'..'z';
  TJvDriveCapacity = (dcDefault, dcSize360kB, dcSize720kB);
  TJvFormatDriveError = (errParams, errSysError, errAborted, errCannotFormat, errOther);
  TJvFormatDriveErrorEvent = procedure(Sender: TObject; Error: TJvFormatDriveError) of object;

  TJvFormatDrive = class(TCommonDialog)
  private
    { Private declarations }
   FAboutJVCL: TJVCLAboutInfo;
    FDrive: TJvDriveLetters;
    FFormatType: TJvFormatType;
    FCapacity: TJvDriveCapacity;
    FHandle: integer;
    IsNT: boolean;
    FOnError: TJvFormatDriveErrorEvent;
    procedure SetDrive(Value: TJvDriveLetters);
  protected
    { Protected declarations }
    procedure DoError(ErrValue: integer);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    function Execute: boolean; override;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Drive: TJvDriveLetters read FDrive write SetDrive default 'A';
    property FormatType: TJvFormatType read FFormatType write FFormatType;
    property Capacity: TJvDriveCapacity read FCapacity write FCapacity;
    property OnError: TJvFormatDriveErrorEvent read FOnError write FOnError;
  end;

  {
  FormatDrive dialog
  hWnd - handle to app,
  Msg - driveID (A = 0, B = 1, C = 2 etc),
  wParam - Capacity ( Win95: 0 - default, 3 - 360/5.25", 5 - 720 / 3.5"
                      WinNT: 0 - default, -,           , - )
  lParam - FormatType (Win95: 0 - quick, 1 - full, 2 - bootable
                       WinNT: 1 - quick, 0 - full, - )
  }
resourcestring
  SNotSupported = 'SHFormatDrive not supported on the current OS';
  
function SHFormatDrive(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

implementation

{ TJvFormatDrive }

constructor TJvFormatDrive.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDrive := 'A';
  IsNt := Win32Platform = VER_PLATFORM_WIN32_NT;
  if AOwner is TCustomForm then
    FHandle := TCustomForm(AOwner).Handle
  else
    FHandle := 0;
end;

function TJvFormatDrive.Execute: boolean;
var iDrive, iCapacity, iFormatType, RetVal: integer;
begin
  if IsNT then
  begin
    iDrive := Ord(FDrive) - Ord('A');
    iCapacity := 0; // other styles not supported
    if FFormatType = ftQuick then
      iFormatType := 1
    else
      iFormatType := 0;
  end
  else
  begin
    iDrive := Ord(FDrive) - Ord('A');
    case FCapacity of
      dcSize360kB: iCapacity := 3;
      dcSize720kB: iCapacity := 5;
    else
      iCapacity := 0;
    end; // case
    iFormatType := Ord(FFormatType);
  end;

  RetVal := SHFormatDrive(FHandle, iDrive, iCapacity, iFormatType);
  if IsNT then
    Result := RetVal = 0
  else
    Result := RetVal = 6;
  if not Result then
    DoError(RetVal);
end;

procedure TJvFormatDrive.DoError(ErrValue: integer);
var err: TJvFormatDriveError;
begin
  if Assigned(FOnError) then
  begin
    if IsNT then
      err := errOther
    else
      case ErrValue of //
        0: err := errParams;
        -1: err := errSysError;
        -2: err := errAborted;
        -3: err := errCannotFormat;
      else
        err := errOther;
      end; // case
    FOnError(self, err);
  end;
end;

procedure TJvFormatDrive.SetDrive(Value: TJvDriveLetters);
begin
  if FDrive <> UpCase(Value) then
    FDrive := UpCase(Value);
end;

function SHFormatDrive; external 'shell32.dll' name 'SHFormatDrive';

end.

