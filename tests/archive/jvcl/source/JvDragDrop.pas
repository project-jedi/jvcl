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
  Windows, Messages, SysUtils, Classes, Controls, ShellAPI, JvComponent;

type
  TJvDropEvent = procedure(Sender: TObject; Pos: TPoint; Value: TStrings) of object;

  TJvDragDrop = class(TJvComponent)
  private
    FAcceptDrag: Boolean;
    FStreamedAcceptDrag: Boolean;
    FFiles: TStrings;
    FOnDrop: TJvDropEvent;
    FIsHooked: Boolean;
    FTargetStrings: TStrings;
    FDropTarget: TWinControl;
    procedure DropFiles(Handle: HDROP);
    procedure SetAcceptDrag(Value: Boolean);
    procedure SetDropTarget(const Value: TWinControl);
    function WndProc(var Msg: TMessage): Boolean;
  protected
    procedure HookControl;
    procedure UnHookControl;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Files: TStrings read FFiles;
    property TargetStrings: TStrings read FTargetStrings write FTargetStrings;
  published
    property AcceptDrag: Boolean read FAcceptDrag write SetAcceptDrag default True;
    property DropTarget: TWinControl read FDropTarget write SetDropTarget;
    property OnDrop: TJvDropEvent read FOnDrop write FOnDrop;
  end;

implementation

uses
  JvWndProcHook;

{ TJvDragDrop }

constructor TJvDragDrop.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptDrag := False;
  FStreamedAcceptDrag := True;
  FFiles := TStringList.Create;
  FIsHooked := False;
  if (Owner is TWinControl) and (csDesigning in ComponentState) then
    FDropTarget := TWinControl(Owner);
end;

destructor TJvDragDrop.Destroy;
begin
  UnHookControl;
  FFiles.Free;
  inherited Destroy;
end;

procedure TJvDragDrop.DropFiles(Handle: HDROP);
var
  Buffer: PChar;
  I, BufferLength, NeededLength: Integer;
  MousePt: TPoint;
  Count: Integer;
begin
  FFiles.Clear;

  BufferLength := MAX_PATH;

  { Note: Do not use fixed stack buffers of size MAX_PATH,
          to prevent buffer overrun attacks, be paranoid <g> }
  GetMem(Buffer, BufferLength);
  try
    { Return value is a count of the dropped files }
    Count := DragQueryFile(Handle, $FFFFFFFF, nil, 0);

    for I := 0 to Count-1 do
    begin
      { Return value is the required size, in characters, of the buffer,
        *not* including the terminating null character (hence the + 1) }
      NeededLength := DragQueryFile(Handle, I, nil, 0) + 1;
      if NeededLength > BufferLength then
      begin
        BufferLength := NeededLength;
        ReallocMem(Buffer, BufferLength);
      end;
      DragQueryFile(Handle, I, Buffer, BufferLength);
      FFiles.Add(Buffer);
    end;
  finally
    FreeMem(Buffer);
  end;

  if Assigned(FTargetStrings) then
    FTargetStrings.Assign(FFiles);

  if Assigned(FOnDrop) then
  begin
    DragQueryPoint(Handle, MousePt);
    FOnDrop(Self, MousePt, FFiles);
  end;

  DragFinish(Handle);
end;

procedure TJvDragDrop.HookControl;
begin
  if FIsHooked then
    Exit;

  { Paranoia checks }
  if Assigned(FDropTarget) and not (csDesigning in ComponentState) then
    FIsHooked := RegisterWndProcHook(FDropTarget, WndProc, hoBeforeMsg);
end;

procedure TJvDragDrop.Loaded;
begin
  inherited Loaded;
  SetAcceptDrag(FStreamedAcceptDrag);
end;

procedure TJvDragDrop.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (AComponent = FDropTarget) and (Operation = opRemove) then
    DropTarget := nil;
end;

procedure TJvDragDrop.SetAcceptDrag(Value: Boolean);
begin
  if csLoading in ComponentState then
    { When loading, delay changing to active until all properties are loaded }
    FStreamedAcceptDrag := Value
  else
    if Value <> FAcceptDrag then
  begin
    FAcceptDrag := Value;

    if Assigned(FDropTarget) and not (csDesigning in ComponentState) then
    begin
      { If the component is being destroyed, we don't want to call it's Handle
        property, which will implicitly re-create it's already destroyed handle }
      if not (csDestroying in FDropTarget.ComponentState) then
        DragAcceptFiles(FDropTarget.Handle, FAcceptDrag);

      if FAcceptDrag then
        HookControl
      else
        UnHookControl;
    end;
  end;
end;

procedure TJvDragDrop.SetDropTarget(const Value: TWinControl);
var
  WasActive: Boolean;
begin
  if csLoading in ComponentState then
    FDropTarget := Value
  else
    if Value <> FDropTarget then
  begin
    WasActive := AcceptDrag;

    { This will implicitly unhook the current DropTarget }
    AcceptDrag := False;

    if Assigned(FDropTarget) then
      FDropTarget.RemoveFreeNotification(Self);

    FDropTarget := Value;

    if Assigned(FDropTarget) then
      FDropTarget.FreeNotification(Self);

    if WasActive then
      { And hook again.. }
      AcceptDrag := True;
  end;
end;

procedure TJvDragDrop.UnHookControl;
begin
  if not FIsHooked then
    Exit;

  FIsHooked := False;

  { Paranoia checks }
  if Assigned(FDropTarget) and not (csDesigning in ComponentState) then
    UnRegisterWndProcHook(FDropTarget, WndProc, hoBeforeMsg);
end;

function TJvDragDrop.WndProc(var Msg: TMessage): Boolean;
begin
  Result := Msg.Msg = WM_DROPFILES;
  if Result then
    DropFiles(HDROP(Msg.WParam))
end;

end.

