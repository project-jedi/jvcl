{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDragDrop.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDragDrop;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ShellApi, JvTypes, JvComponent;

type
  TJvDragDrop = class(TJvComponent)
  private
    FAccept: Boolean;
    FHandle: THandle;
    FFiles: TStringList;
    FOnDrop: TDropEvent;
    procedure DropFiles(Handle: HDrop);
    procedure SetAccept(Value: Boolean);
    function WndProc(var Msg: TMessage):boolean;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Files: TStringList read FFiles;
  published
    property AcceptDrag: Boolean read FAccept write SetAccept default True;
    property OnDrop: TDropEvent read FOnDrop write FOnDrop;
  end;

implementation
uses
  JvWndProcHook;

{*****************************************************}

constructor TJvDragDrop.Create(AOwner: TComponent);
var
  WinCtl: TWinControl;
begin
  inherited;
  FAccept := True;
  FFiles := TStringList.Create;
  if not (csDesigning in ComponentState) then
  begin
    if (Owner is TWinControl) then
    begin
      FHandle := TWinControl(Owner).Handle;
      RegisterWndProcHook(TWinControl(Owner),WndProc,hoBeforeMsg);
    end
    else
      FAccept := False;
  end;
  SetAccept(FAccept);
end;

{*****************************************************}

destructor TJvDragDrop.Destroy;
begin
  FFiles.Free;
  if (Owner is TWinControl) and not (csDesigning in ComponentState) then
    UnregisterWndProcHook(TWinControl(Owner),WndProc,hoBeforeMsg);
  inherited;
end;

{*****************************************************}

procedure TJvDragDrop.SetAccept(Value: Boolean);
begin
  FAccept := Value;
  if not (csDesigning in ComponentState) then
    DragAcceptFiles(FHandle, FAccept);
end;

{*****************************************************}

function TJvDragDrop.WndProc(var Msg: TMessage):boolean;
begin
  Result := Msg.Msg = WM_DROPFILES;
  if Result then
    DropFiles(HDrop(Msg.wParam))
end;

{*****************************************************}

procedure TJvDragDrop.DropFiles(Handle: HDrop);
var
  pszFileWithPath, pszFile: PChar;
  iFile, iStrLen, iTempLen: Integer;
  MousePt: TPoint;
  count: Integer;
begin
  FFiles.Clear;
  iStrLen := 128;
  pszFileWithPath := StrAlloc(iStrLen);
  pszFile := StrAlloc(iStrLen);
  count := DragQueryFile(Handle, $FFFFFFFF, pszFile, iStrLen);
  iFile := 0;
  while iFile < count do
  begin
    iTempLen := DragQueryFile(Handle, iFile, nil, 0) + 1;
    if iTempLen > iStrLen then
    begin
      iStrLen := iTempLen;
      StrDispose(pszFileWithPath);
      pszFileWithPath := StrAlloc(iStrLen);
    end;
    DragQueryFile(Handle, iFile, pszFileWithPath, iStrLen);
    FFiles.Add(StrPas(pszFileWithPath));
    Inc(iFile);
  end;
  StrDispose(pszFileWithPath);
  if Assigned(FOnDrop) then
  begin
    DragQueryPoint(Handle, MousePt);
    FOnDrop(Self, MousePt, FFiles);
  end;
  DragFinish(Handle);
end;

end.
