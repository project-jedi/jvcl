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
  Windows, Messages, SysUtils, Classes, Controls, ShellApi,
  JvTypes, JvComponent;

type
  TJvDragDrop = class(TJvComponent)
  private
    FAcceptDrag: Boolean;
    { (rb) Don't remember FHandle, it may be changed, for instance by
      setting FormStyle }
    //FHandle: THandle;
    FFiles: TStringList;
    FOnDrop: TDropEvent;
    FIsHooked: Boolean;
    procedure DropFiles(Handle: HDROP);
    procedure SetAcceptDrag(Value: Boolean);
    function WndProc(var Msg: TMessage): Boolean;
  protected
    procedure HookControl;
    procedure UnHookControl;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { (rb) Should be changed to TStrings }
    property Files: TStringList read FFiles;
  published
    property AcceptDrag: Boolean read FAcceptDrag write SetAcceptDrag default True;
    property OnDrop: TDropEvent read FOnDrop write FOnDrop;
  end;

implementation

uses
  JvWndProcHook;

constructor TJvDragDrop.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptDrag := True;
  FFiles := TStringList.Create;
  FIsHooked := False;
end;

destructor TJvDragDrop.Destroy;
begin
  FFiles.Free;
  { (rb) Actually Owner now = nil, thus the unhooking is already done, in the
    function TJvWndProcHook.Notification }
  UnHookControl;
  inherited Destroy;
end;

procedure TJvDragDrop.SetAcceptDrag(Value: Boolean);
begin
  FAcceptDrag := Value;
  if [csDesigning, csLoading] * ComponentState <> [] then
    Exit;

  if Owner is TWinControl then
  begin
    DragAcceptFiles(TWinControl(Owner).Handle, FAcceptDrag);
    if FAcceptDrag then
      HookControl
    else
      UnHookControl;
  end;
end;

function TJvDragDrop.WndProc(var Msg: TMessage): boolean;
begin
  Result := Msg.Msg = WM_DROPFILES;
  if Result then
    DropFiles(HDrop(Msg.wParam))
end;

procedure TJvDragDrop.DropFiles(Handle: HDROP);
var
  pszFileWithPath, pszFile: PChar;
  iFile, iStrLen, iTempLen: Integer;
  MousePt: TPoint;
  Count: Integer;
begin
  FFiles.Clear;
  iStrLen := 128;
  pszFileWithPath := StrAlloc(iStrLen);
  pszFile := StrAlloc(iStrLen);
  Count := DragQueryFile(Handle, $FFFFFFFF, pszFile, iStrLen);
  iFile := 0;
  while iFile < Count do
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

procedure TJvDragDrop.Loaded;
begin
  inherited Loaded;
  SetAcceptDrag(FAcceptDrag);
end;

procedure TJvDragDrop.HookControl;
begin
  if FIsHooked then Exit;

  if (Owner is TWinControl) and not (csDesigning in ComponentState) then
    FIsHooked := RegisterWndProcHook(TWinControl(Owner), WndProc, hoBeforeMsg);
end;

procedure TJvDragDrop.UnHookControl;
begin
  if not FIsHooked then Exit;

  FIsHooked := False;

  if (Owner is TWinControl) and not (csDesigning in ComponentState) then
    UnRegisterWndProcHook(TWinControl(Owner), WndProc, hoBeforeMsg);
end;

end.

