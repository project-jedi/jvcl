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

unit JvSHFmt;

{ A component that shows the "format floppy" dialog in Windows }

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs,
  JVCLVer;

type
  TJvFormatType = (ftQuick, ftStandard, ftBootable);
  //TJvDriveLetters = 'A'..'Z';
  TJvDriveCapacity = (dcDefault, dcSize360kB, dcSize720kB);
  TJvFormatDriveError = (errParams, errSysError, errAborted, errCannotFormat, errOther);
  TJvFormatDriveErrorEvent = procedure(Sender: TObject; Error: TJvFormatDriveError) of object;

  TJvFormatDrive = class(TCommonDialog)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FDrive: Char;
    FFormatType: TJvFormatType;
    FCapacity: TJvDriveCapacity;
    FHandle: Integer;
    FIsNT: Boolean;
    FOnError: TJvFormatDriveErrorEvent;
    procedure SetDrive(Value: Char);
  protected
    procedure DoError(ErrValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Drive: Char read FDrive write SetDrive default 'A';
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

function SHFormatDrive(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

implementation

resourcestring
  SNotSupported = 'SHFormatDrive not supported on current OS';

constructor TJvFormatDrive.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDrive := 'A';
  FIsNT := Win32Platform = VER_PLATFORM_WIN32_NT;
  if AOwner is TCustomForm then
    FHandle := TCustomForm(AOwner).Handle
  else
    FHandle := 0;
end;

function TJvFormatDrive.Execute: Boolean;
var
  iDrive, iCapacity, iFormatType, RetVal: Integer;
begin
  if FIsNT then
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
      dcSize360kB:
        iCapacity := 3;
      dcSize720kB:
        iCapacity := 5;
    else
      iCapacity := 0;
    end;
    iFormatType := Ord(FFormatType);
  end;

  RetVal := SHFormatDrive(FHandle, iDrive, iCapacity, iFormatType);
  if FIsNT then
    Result := RetVal = 0
  else
    Result := RetVal = 6;
  if not Result then
    DoError(RetVal);
end;

procedure TJvFormatDrive.DoError(ErrValue: Integer);
var
  Err: TJvFormatDriveError;
begin
  if Assigned(FOnError) then
  begin
    if FIsNT then
      Err := errOther
    else
      case ErrValue of
        0:
          Err := errParams;
        -1:
          Err := errSysError;
        -2:
          Err := errAborted;
        -3:
          Err := errCannotFormat;
      else
        Err := errOther;
      end; // case
    FOnError(Self, Err);
  end;
end;

procedure TJvFormatDrive.SetDrive(Value: Char);
begin
  // (rom) secured
  Value := UpCase(Value);
  if Value in ['A'..'Z'] then
    FDrive := UpCase(Value);
end;

function SHFormatDrive; external 'shell32.dll' name 'SHFormatDrive';

end.

