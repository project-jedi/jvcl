{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDirectorySpy.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvDirectorySpy;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  JvTypes, JvComponent;

type
  TJvDirectoryThread = class(TThread)
  protected
    procedure Draw;
    procedure Execute; override;
    function GetLastErrorMsg: string;
  public
    FDelay: Cardinal;
    FOnDraw: TNotifyEvent;
    FMask: TJvDirMasks;
    FDir: string;
    FRecu: Boolean;
  end;

  TJvDirectorySpy = class(TJvComponent)
  private
    FActive: Boolean;
    FMask: TJvDirMasks;
    FDir: string;
    FOnChange: TNotifyEvent;
    FRecursive: Boolean;
    FThread: TJvDirectoryThread;
    procedure SetActive(const Value: Boolean);
    procedure SetMask(const Value: TJvDirMasks);
    procedure SetDir(const Value: string);
    procedure SetRecursive(const Value: Boolean);
    procedure Refresh;
    procedure Changed(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Mask: TJvDirMasks read FMask write SetMask default [dmFileNameChange, dmDirnameChange, dmAttributesChange,
      dmSizeChange, dmLastWriteChange];
    property Active: Boolean read FActive write SetActive default False;
    property Directory: string read FDir write SetDir;
    property Recursive: Boolean read FRecursive write SetRecursive default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvDirectorySpy
///////////////////////////////////////////////////////////

procedure TJvDirectorySpy.Changed(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{**************************************************}

constructor TJvDirectorySpy.Create(AOwner: TComponent);
begin
  inherited;
  FMask := [dmFileNameChange, dmDirnameChange, dmAttributesChange, dmSizeChange, dmLastWriteChange];
  FActive := False;
  FRecursive := True;

  // (rom) better get the drive Windows started from
  FDir := 'c:\';
  FThread := TJvDirectoryThread.Create(True);
  FThread.FMask := FMask;
  FThread.FOnDraw := Changed;
  FThread.FDir := FDir;
  FThread.FRecu := True;
end;

{**************************************************}

destructor TJvDirectorySpy.Destroy;
begin
  FThread.FreeOnTerminate := True;
  FThread.Terminate;
  inherited;
end;

{**************************************************}

procedure TJvDirectorySpy.Refresh;
begin
  //Refresh the thread
  if not (csDesigning in ComponentState) then
  begin
    FThread.FreeOnTerminate := True;
    FThread.Terminate;

    FThread := TJvDirectoryThread.Create(True);
    FThread.FMask := FMask;
    FThread.FOnDraw := Changed;
    FThread.FDir := FDir;
    FThread.FRecu := FRecursive;
    if FDir <> '' then
      FThread.Resume;
  end;
end;

{**************************************************}

procedure TJvDirectorySpy.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
    begin
      if not (csDesigning in ComponentState) then
        FThread.Resume
    end
    else
      FThread.Suspend;
  end;
end;

{**************************************************}

procedure TJvDirectorySpy.SetDir(const Value: string);
begin
  if FDir <> Value then
  begin
    FDir := Value;
    Refresh;
  end;
end;

{**************************************************}

procedure TJvDirectorySpy.SetMask(const Value: TJvDirMasks);
begin
  if FMask <> Value then
  begin
    FMask := Value;
    Refresh;
  end;
end;

{**************************************************}

procedure TJvDirectorySpy.SetRecursive(const Value: Boolean);
begin
  if FRecursive <> Value then
  begin
    FRecursive := Value;
    Refresh;
  end;
end;

///////////////////////////////////////////////////////////
// TJvDirectoryThread
///////////////////////////////////////////////////////////

procedure TJvDirectoryThread.Draw;
begin
  if Assigned(FOnDraw) then
    FOnDraw(Self);
end;

{**************************************************}

procedure TJvDirectoryThread.Execute;
var
  h: THandle;
  filter: Longint;
  Flag: LongBool;
begin
  if FDir = '' then
    Exit;
  filter := 0;
  if dmFileNameChange in FMask then
    filter := FILE_NOTIFY_CHANGE_FILE_NAME;
  if dmDirnameChange in FMask then
    filter := filter or FILE_NOTIFY_CHANGE_DIR_NAME;
  if dmAttributesChange in FMask then
    filter := filter or FILE_NOTIFY_CHANGE_ATTRIBUTES;
  if dmSizeChange in FMask then
    filter := filter or FILE_NOTIFY_CHANGE_SIZE;
  if dmLastWriteChange in FMask then
    filter := filter or FILE_NOTIFY_CHANGE_LAST_WRITE;
  if dmSecurityChange in FMask then
    filter := filter or FILE_NOTIFY_CHANGE_SECURITY;

  if FRecu then
    Flag := LongBool(1)
  else
    Flag := LongBool(0);

  h := FindFirstChangeNotification(PChar(FDir), Flag, filter);
  if h = INVALID_HANDLE_VALUE then
    raise EJvDirectoryError.Create(GetLastErrorMsg);

  repeat
    if WaitForSingleObject(h, 1000) = WAIT_OBJECT_0 then
    begin
      Synchronize(Draw);
      if not FindNextChangeNotification(h) then
        raise EJvDirectoryError.Create(GetLastErrorMsg);
    end;
  until Terminated;

  FindCloseChangeNotification(h);
end;

{**************************************************}

function TJvDirectoryThread.GetLastErrorMsg: string;
var
  msg: array[0..1000] of Char;
begin
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError, 0, msg, 1000, nil);
  Result := msg;
end;

end.
