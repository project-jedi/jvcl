{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFileUtils.PAS, released on 2001-02-28.

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

unit JvFileUtils;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ShellApi, JvTypes, JvComponent;

type
  TJvFilesUtils = class(TJvComponent)
  private
    FTitle: string;
    FAllowUndo: Boolean;
    FOnCollision: Boolean;
    FConfirmation: Boolean;
    FSilent: Boolean;
    FFilesOnly: Boolean;
  published
    property Title: string read FTitle write FTitle;
    property AllowUndo: Boolean read FAllowUndo write FAllowUndo;
    property RenameOnCollision: Boolean read FOnCollision write FOnCollision;
    property Confirmation: Boolean read FConfirmation write FConfirmation;
    property Silent: Boolean read FSilent write FSilent;
    property FilesOnly: Boolean read FFilesOnly write FFilesOnly;
    function DeleteFile(FileName: string): Boolean;
    procedure DeleteFileToBin(FileName: string);
    function CopyFile(From, Destination: string): Boolean;
    function MoveFile(From, Destination: string): Boolean;
    function RenameFile(From, Destination: string): Boolean;
  end;

implementation

{**********************************************************}

function TJvFilesUtils.DeleteFile(FileName: string): Boolean;
var
  Sh: TSHFileOpStruct;
begin
  with Sh do
  begin
    Wnd := Application.Handle;
    wFunc := FO_DELETE;
    pFrom := PChar(FileName);
    lpszProgressTitle := PChar(FTitle);
    fFlags := 0;
    if FAllowUndo then
      fFlags := fFlags + FOF_ALLOWUNDO;
    if FOnCollision then
      fFlags := fFlags + FOF_RENAMEONCOLLISION;
    if not FConfirmation then
      fFlags := fFlags + FOF_NOCONFIRMATION;
    if FSilent then
      fFlags := fFlags + FOF_SILENT;
    if FFilesOnly then
      fFlags := fFlags + FOF_FILESONLY;
  end;
  Result := SHFileOperation(Sh) = 0;
end;

{**********************************************************}

procedure TJvFilesUtils.DeleteFileToBin(FileName: string);
var
  Sh: TSHFileOpStruct;
begin
  with Sh do
  begin
    Wnd := 0;
    wFunc := FO_DELETE;
    pFrom := PChar(FileName);
    fFlags := FOF_ALLOWUNDO;
  end;
  SHFileOperation(Sh);
end;

{**********************************************************}

function TJvFilesUtils.CopyFile(From, Destination: string): Boolean;
var
  Sh: TSHFileOpStruct;
begin
  with Sh do
  begin
    Wnd := Application.Handle;
    wFunc := FO_COPY;
    pFrom := PChar(From);
    pTo := PChar(Destination);
    lpszProgressTitle := PChar(FTitle);
    fFlags := 0;
    if FAllowundo then
      fFlags := fFlags + FOF_ALLOWUNDO;
    if FOnCollision then
      fFlags := fFlags + FOF_RENAMEONCOLLISION;
    if not FConfirmation then
      fFlags := fFlags + FOF_NOCONFIRMATION;
    if FSilent then
      fFlags := fFlags + FOF_SILENT;
    if FFilesOnly then
      fFlags := fFlags + FOF_FILESONLY;
  end;
  Result := SHFileOperation(Sh) = 0;
end;

{**********************************************************}

function TJvFilesUtils.MoveFile(From, Destination: string): Boolean;
var
  Sh: TSHFileOpStruct;
begin
  with Sh do
  begin
    Wnd := Application.Handle;
    wFunc := FO_MOVE;
    pFrom := PChar(From);
    pTo := PChar(Destination);
    lpszProgressTitle := PChar(FTitle);
    fFlags := 0;
    if FAllowundo then
      fFlags := fFlags + FOF_ALLOWUNDO;
    if FOnCollision then
      fFlags := fFlags + FOF_RENAMEONCOLLISION;
    if not FConfirmation then
      fFlags := fFlags + FOF_NOCONFIRMATION;
    if FSilent then
      fFlags := fFlags + FOF_SILENT;
    if FFilesOnly then
      fFlags := fFlags + FOF_FILESONLY;
  end;
  Result := SHFileOperation(Sh) = 0;
end;

{**********************************************************}

function TJvFilesUtils.RenameFile(From, Destination: string): Boolean;
var
  Sh: TSHFileOpStruct;
begin
  with Sh do
  begin
    Wnd := Application.Handle;
    wFunc := FO_RENAME;
    pFrom := PChar(From);
    pTo := PChar(Destination);
    lpszProgressTitle := PChar(FTitle);
    fFlags := 0;
    if FAllowundo then
      fFlags := fFlags + FOF_ALLOWUNDO;
    if FOnCollision then
      fFlags := fFlags + FOF_RENAMEONCOLLISION;
    if not FConfirmation then
      fFlags := fFlags + FOF_NOCONFIRMATION;
    if FSilent then
      fFlags := fFlags + FOF_SILENT;
    if FFilesOnly then
      fFlags := fFlags + FOF_FILESONLY;
  end;
  Result := SHFileOperation(Sh) = 0;
end;

end.
