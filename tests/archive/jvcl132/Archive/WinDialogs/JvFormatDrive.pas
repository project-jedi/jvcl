{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormatDrive.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvFormatDrive;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvBaseDlg, JvTypes;

type
  TJvFormatDrive = class(TJvCommonDialog)
  private
    FOption: TFormatOption;
    FOnCancel: TNotifyEvent;
    FOnError: TNotifyEvent;
    FOnNoFormat: TNotifyEvent;
    FOnSuccess: TNotifyEvent;
    FHandle: THandle;
    FDrive: Char;
    FSHFormatDrive: TSHFormatDrive;
    FDllHandle: THandle;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Option: TFormatOption read FOption write FOption default shFull;
    property OnError: TNotifyEvent read FOnError write FonError;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnNoformat: TNotifyEvent read FOnNoFormat write FOnNoFormat;
    property OnSuccess: TNotifyEvent read FOnSuccess write FOnSuccess;
    property Drive: Char read FDrive write FDrive default 'A';
    function Execute: Boolean; override;
    function FormatDrive(Drive: Char): Boolean;
  end;

implementation

{*********************************************************}

constructor TJvFormatDrive.Create(AOwner: TComponent);
begin
  inherited;
  FOption := shFull;
  if AOwner is TForm then
    FHandle := (AOwner as TForm).Handle
  else
    FHandle := Application.Handle;
  FDrive := 'A';
  if not (csDesigning in ComponentState) then
  begin
    FDllHandle := LoadLibrary('SHELL32.DLL');
    if FDllHandle <> 0 then
      FSHFormatDrive := GetProcAddress(FDllHandle, 'SHFormatDrive')
    else
      FSHFormatDrive := nil;
  end
  else
    FDllHandle := 0;
end;

{*********************************************************}

destructor TJvFormatDrive.Destroy;
begin
  if FDllHandle <> 0 then
    FreeLibrary(FDllHandle);
  inherited;
end;

{*********************************************************}

function TJvFormatDrive.Execute: Boolean;
begin
  if not (csDesigning in ComponentState) then
    Result := FormatDrive(FDrive)
  else
  begin
    ShowMessage('Unavailable in Design Mode');
    Result := False;
  end;
end;

{*********************************************************}

function TJvFormatDrive.FormatDrive(Drive: Char): Boolean;
var
  d: WORD;
  o: WORD;
begin
  Result := False;
  Drive := LowerCase(Drive)[1];
  if Ord(Drive) <= Ord('z') then
  begin
    d := Ord(Drive) - Ord('a');
    case FOption of
      shQuickFormat:
        o := SHFMT_OPT_QUICKFORMAT;
      shFull:
        o := SHFMT_OPT_FULL;
      shSystemFilesOnly:
        o := SHFMT_OPT_SYSONLY;
    else
      o := SHFMT_OPT_FULL;
    end;
    case FSHFormatDrive(FHandle, d, SHFMT_ID_DEFAULT, o) of
      SHFMT_ERROR:
        if Assigned(FOnError) then
          FOnError(Self);
      SHFMT_CANCEL:
        if Assigned(FOnCancel) then
          FOnCancel(Self);
      SHFMT_NOFORMAT:
        if Assigned(FOnNoFormat) then
          FOnNoFormat(Self);
    else
      begin
        Result := True;
        if Assigned(FOnSuccess) then
          FOnSuccess(Self);
      end;
    end;
  end;
end;

end.
