{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFixedEditPopUp.PAS, released 2003-03-01.

The Initial Developer of the Original Code is Peter Thornqvist (peter3 at sourceforge dot net)
Portions created by Peter Thornqvist are Copyright (C) 2002 Peter Thornqvist .
All Rights Reserved.

Contributor(s):
Steve Magruder
Remko Bonte

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  A unit that can be used to replace the default system popup menu for edit controls. The problem with
  MS implementation is that the Paste command is enabled even when the edit is set to read-only

  By overriding TCustomEdit.GetPopupMenu (virtual), you can return an instance of this popup menu
  instead of the default, i.e:
  interface:

    function GetPopupMenu: TPopupMenu; override;

  implementation:

    function TMyEdit.GetPopupMenu: TPopupMenu;
    begin
      Result := inherited GetPopupMenu;
      if Result = nil then // user has not assigned his own popup menu, so use fixed default
        Result := FixedDefaultEditPopUp(Self);
    end;

  The popup is constructed as a singelton shared between all edit controls using it, so it is
  as resource friendly as we could make it and you should NOT free it after use. The popup is
  not created until first use, so if you don't use it, it doesn't take any resources.

  The popup automatically handles cut, copy, paste, select all, clear and undo events and it's aware
  of and can also handle the ClipboardCommands property in some JVCL edits. Menu items
  are automatically enabled / disabled according to the current state of the edit.

  The popup is "self-translating" based on Windows locale. If you want to
  use resourcestrings and supply your own translations, call FixedDefaultEditPopUseResourceString(True);
  (yes, the name is that long on purpose) *before the first call* to FixedDefaultEditPopUp.

  UPDATE 2003-07-14:
    Rewritten to handle any TWinControl descendant. To make a TWinControl
    component compatible it should implement the following messages and styles:
      * If the control is readonly, it should set ES_READONLY in GWL_STYLE
        (this can be done using the EM_SETREADONLY message for edit descendants)
      * If text can be selected, the control should implement the EM_GETSEL message
      * If the control has undo capability, it should implement the EM_CANUNDO message

    The control should also react to the following messages:
      * Undo: WM_UNDO
      * Cut: WM_CUT
      * Copy: WM_COPY
      * Paste: WM_PASTE
      * Clear: WM_CLEAR
      * Select All: EM_SETSEL with wParam=0 and lParam=-1

History:
3.0:
  2003-09-19:
    - introduced IFixedPopupIntf
    - speed optimation in THiddenPopupObject.GetPopupMenu
-----------------------------------------------------------------------------}
// $Id$

unit JvFixedEditPopUp;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Menus, TypInfo,
  JvTypes;

type
  { IFixedPopupIntf is implemented by a component that supports the
    FixedPopupMenu. }
  IFixedPopupIntf = interface
    ['{2ECA1438-EFA5-460A-B586-C30C04B85FF3}']
    function CanUndo: Boolean;
    function CanRedo: Boolean; // unused
    function CanCut: Boolean;
    function CanCopy: Boolean;
    function CanPaste: Boolean;
    function CanSelectAll: Boolean;
    function HasSelection: Boolean;
    procedure Undo;
    procedure Redo; // unused
    procedure Cut;
    procedure Copy;
    procedure Paste;
    { Delete() deletes the selected text without storing the content to the
      clipboard. It's enabled/disabled in the same way as Cut. }
    procedure Delete;
    procedure SelectAll;
  end;

// Returns a popup menu with the standard actions associated with edit controls (Undo, Cut, Copy, Paste, Delete, Select All).
// The actions are handled autmatically by sending messages (WM_COPY, WM_CUT etc) to the control
function FixedDefaultEditPopup(AEdit: TWinControl; Update: Boolean = True): TPopupMenu;
// Call with Value set to True to use the internal resourcestrings instead of those
// provided by Windows. These strings can subsequently be translated using the ITE.
// By default, the Windows provided strings are used.
procedure FixedDefaultEditPopUseResourceString(Value: Boolean);

// Updates the menu items enabled property
procedure FixedDefaultEditPopupUpdate(AEdit: TWinControl);

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvResources;

type
  THiddenPopupObject = class(TComponent)
  private
    FEdit: TWinControl;
    FPopupMenu: TPopupMenu;
    procedure GetDefaultMenuCaptions;
    procedure DoSelectAll(Sender: TObject);
    procedure DoUndo(Sender: TObject);
    procedure DoDelete(Sender: TObject);
    procedure DoPaste(Sender: TObject);
    procedure DoCut(Sender: TObject);
    procedure DoCopy(Sender: TObject);
    function CanUndo: Boolean;
    function ReadOnly: Boolean;
    function GetTextLen: Integer;
    function SelLength: Integer;
    //function GetPopupMenu: TPopupMenu;
    function GetPopupMenuEx(Update: Boolean): TPopupMenu;
    procedure SetEdit(const Value: TWinControl);
    function GetClipboardCommands: TJvClipboardCommands;
    procedure UpdateItems;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property Edit: TWinControl read FEdit write SetEdit;
    // property PopupMenu: TPopupMenu read GetPopupMenu;
  end;

var
  GlobalHiddenPopup: THiddenPopupObject = nil;
  GlobalUseResourceStrings: Boolean = False;

function FixedDefaultEditPopup(AEdit: TWinControl; Update: Boolean = True): TPopupMenu;
begin
  if GlobalHiddenPopup = nil then
    GlobalHiddenPopup := THiddenPopupObject.Create(nil);
  GlobalHiddenPopup.Edit := AEdit;
  Result := GlobalHiddenPopup.GetPopupMenuEx(Update);
end;

procedure FixedDefaultEditPopUseResourceString(Value: Boolean);
begin
  GlobalUseResourceStrings := Value;
end;

procedure FixedDefaultEditPopupUpdate(AEdit: TWinControl);
begin
  if (GlobalHiddenPopup <> nil) and (GlobalHiddenPopup.Edit = AEdit) then
    GlobalHiddenPopup.UpdateItems;
end;

//=== { THiddenPopupObject } =================================================

function THiddenPopupObject.CanUndo: Boolean;
begin
  if (Edit <> nil) and Edit.HandleAllocated then
    Result := SendMessage(Edit.Handle, EM_CANUNDO, 0, 0) <> 0
  else
    Result := False;
end;

procedure THiddenPopupObject.DoCopy(Sender: TObject);
var
  PopupIntf: IFixedPopupIntf;
begin
  if Assigned(Edit) and Edit.HandleAllocated then
    if Edit.GetInterface(IFixedPopupIntf, PopupIntf) then
      PopupIntf.Copy
    else
      Edit.Perform(WM_COPY, 0, 0);
end;

procedure THiddenPopupObject.DoCut(Sender: TObject);
var
  PopupIntf: IFixedPopupIntf;
begin
  if Assigned(Edit) and Edit.HandleAllocated then
    if Edit.GetInterface(IFixedPopupIntf, PopupIntf) then
      PopupIntf.Cut
    else
      Edit.Perform(WM_CUT, 0, 0);
end;

procedure THiddenPopupObject.DoDelete(Sender: TObject);
var
  PopupIntf: IFixedPopupIntf;
begin
  if Assigned(Edit) and Edit.HandleAllocated then
    if Edit.GetInterface(IFixedPopupIntf, PopupIntf) then
      PopupIntf.Delete
    else
      Edit.Perform(WM_CLEAR, 0, 0);
end;

procedure THiddenPopupObject.DoPaste(Sender: TObject);
var
  PopupIntf: IFixedPopupIntf;
begin
  if Assigned(Edit) and Edit.HandleAllocated then
    if Edit.GetInterface(IFixedPopupIntf, PopupIntf) then
      PopupIntf.Paste
    else
      Edit.Perform(WM_PASTE, 0, 0);
end;

procedure THiddenPopupObject.DoSelectAll(Sender: TObject);
var
  PopupIntf: IFixedPopupIntf;
begin
  if Assigned(Edit) and Edit.HandleAllocated then
    if Edit.GetInterface(IFixedPopupIntf, PopupIntf) then
      PopupIntf.SelectAll
    else
      Edit.Perform(EM_SETSEL, 0, -1);
end;

procedure THiddenPopupObject.DoUndo(Sender: TObject);
var
  PopupIntf: IFixedPopupIntf;
begin
  if Assigned(Edit) and Edit.HandleAllocated then
  begin
    if Edit.GetInterface(IFixedPopupIntf, PopupIntf) then
      PopupIntf.Undo
    else
      Edit.Perform(WM_UNDO, 0, 0);
  end;
end;

type
  TIntegerSet = set of 0..SizeOf(Integer) * 8 - 1;

function THiddenPopupObject.GetClipboardCommands: TJvClipboardCommands;
const
  cClipboardCommands = 'ClipboardCommands';
var
  Value: TIntegerSet;
  I: Integer;
begin
  if IsPublishedProp(Edit, cClipboardCommands) then
  begin
    Result := [];
    // does it really have to be this complicated ?!
    Integer(Value) := GetOrdProp(Edit, cClipboardCommands);
    for I := 0 to SizeOf(Integer) * 8 - 1 do
      if I in Value then
        Include(Result, TJvClipboardCommand(I));
  end
  else
    Result := [caCopy, caCut, caPaste, caUndo];
end;

procedure THiddenPopupObject.GetDefaultMenuCaptions;
var
  H: HMODULE;
  hMenu, hSubMenu: THandle;
  Buf: array [0..255] of Char;
begin
  // get the translated captions from Windows' own default popup:
  H := GetModuleHandle('user32.dll');
  hMenu := LoadMenu(H, MakeIntResource(1));
  if hMenu = 0 then
    Exit;
  try
    hSubMenu := GetSubMenu(hMenu, 0);
    if hSubMenu = 0 then
      Exit;

    if GetMenuString(hSubMenu, WM_UNDO, Buf, SizeOf(Buf), MF_BYCOMMAND) <> 0 then
      FPopupMenu.Items[0].Caption := Buf;
    if GetMenuString(hSubMenu, WM_CUT, Buf, SizeOf(Buf), MF_BYCOMMAND) <> 0 then
      FPopupMenu.Items[2].Caption := Buf;
    if GetMenuString(hSubMenu, WM_COPY, Buf, SizeOf(Buf), MF_BYCOMMAND) <> 0 then
      FPopupMenu.Items[3].Caption := Buf;
    if GetMenuString(hSubMenu, WM_PASTE, Buf, SizeOf(Buf), MF_BYCOMMAND) <> 0 then
      FPopupMenu.Items[4].Caption := Buf;
    if GetMenuString(hSubMenu, WM_CLEAR, Buf, SizeOf(Buf), MF_BYCOMMAND) <> 0 then
      FPopupMenu.Items[5].Caption := Buf;
    if GetMenuString(hSubMenu, EM_SETSEL, Buf, SizeOf(Buf), MF_BYCOMMAND) <> 0 then
      FPopupMenu.Items[7].Caption := Buf;
  finally
    DestroyMenu(hMenu);
  end;
end;

{function THiddenPopupObject.GetPopupMenu: TPopupMenu;
begin
  Result := GetPopupMenuEx(True);
end;}

function THiddenPopupObject.GetPopupMenuEx(Update: Boolean): TPopupMenu;
var
  m: TMenuItem;
begin
  if FPopupMenu = nil then
  begin
    FPopupMenu := TPopupMenu.Create(Self);
    { build menu:
      Undo
      -
      Cut
      Copy
      Paste
      Delete
      -
      Select All
    }

    // start off with resourcestrings (in case GetDefaultMenuCaptions fails)
    m := TMenuItem.Create(Self);
    m.Caption := RsUndoItem;
    m.OnClick := DoUndo;
    FPopupMenu.Items.Add(m);

    m := TMenuItem.Create(Self);
    m.Caption := '-';
    FPopupMenu.Items.Add(m);

    m := TMenuItem.Create(Self);
    m.Caption := RsCutItem;
    m.OnClick := DoCut;
    FPopupMenu.Items.Add(m);

    m := TMenuItem.Create(Self);
    m.Caption := RsCopyItem;
    m.OnClick := DoCopy;
    FPopupMenu.Items.Add(m);

    m := TMenuItem.Create(Self);
    m.Caption := RsPasteItem;
    m.OnClick := DoPaste;
    FPopupMenu.Items.Add(m);

    m := TMenuItem.Create(Self);
    m.Caption := RsDeleteItem;
    m.OnClick := DoDelete;
    FPopupMenu.Items.Add(m);

    m := TMenuItem.Create(Self);
    m.Caption := '-';
    FPopupMenu.Items.Add(m);

    m := TMenuItem.Create(Self);
    m.Caption := RsSelectAllItem;
    m.OnClick := DoSelectAll;
    FPopupMenu.Items.Add(m);

    if not GlobalUseResourceStrings then
      GetDefaultMenuCaptions;
  end;
  if Update then
    UpdateItems;
  Result := FPopupMenu;
end;

procedure THiddenPopupObject.UpdateItems;
var
  cc: TJvClipboardCommands;
  ASelLength: Integer;
  AReadOnly: Boolean;
  ATextLen: Integer;
  PopupIntf: IFixedPopupIntf;
begin
  if (Edit <> nil) and Edit.HandleAllocated then
  begin
    cc := GetClipboardCommands;
    FPopupMenu.PopupComponent := Edit;

    if Edit.GetInterface(IFixedPopupIntf, PopupIntf) then
    begin
      // undo
      FPopupMenu.Items[0].Enabled := (caUndo in cc) and PopupIntf.CanUndo;
      // cut
      FPopupMenu.Items[2].Enabled := (caCut in cc) and PopupIntf.HasSelection and PopupIntf.CanCut;
      // copy
      FPopupMenu.Items[3].Enabled := (caCopy in cc) and PopupIntf.HasSelection and PopupIntf.CanCopy;
      // paste
      FPopupMenu.Items[4].Enabled := (caPaste in cc) and PopupIntf.CanPaste;
      // delete
      FPopupMenu.Items[5].Enabled := PopupIntf.HasSelection and PopupIntf.CanCut;
      // select all
      FPopupMenu.Items[7].Enabled := PopupIntf.CanSelectAll;
    end
    else
    begin
      ASelLength := SelLength;
      AReadOnly := ReadOnly;
      ATextLen := GetTextLen;

      // undo
      FPopupMenu.Items[0].Enabled := (caUndo in cc) and CanUndo;
      // cut
      FPopupMenu.Items[2].Enabled := (ASelLength > 0) and not AReadOnly and (caCut in cc);
      // copy
      FPopupMenu.Items[3].Enabled := (ASelLength > 0) and (caCopy in cc);
      // paste
      FPopupMenu.Items[4].Enabled := not AReadOnly and (caPaste in cc);
      // delete
      FPopupMenu.Items[5].Enabled := (ASelLength > 0) and not AReadOnly { and (caCut in cc)};
      // select all
      FPopupMenu.Items[7].Enabled := (ATextLen > 0) and (ASelLength <> ATextLen);
    end;
  end;
end;

function THiddenPopupObject.GetTextLen: Integer;
begin
  if (Edit <> nil) and Edit.HandleAllocated then
    Result := Edit.GetTextLen
  else
    Result := 0;
end;

function THiddenPopupObject.ReadOnly: Boolean;
begin
  if (Edit <> nil) and Edit.HandleAllocated then
    Result := GetWindowLong(Edit.Handle, GWL_STYLE) and ES_READONLY = ES_READONLY
  else
    Result := False;
end;

function THiddenPopupObject.SelLength: Integer;
var
  StartPos, EndPos: Integer;
  MsgResult: Longint;
begin
  Result := 0;
  if (Edit <> nil) and Edit.HandleAllocated then
  begin
    StartPos := 0;
    EndPos := 0;
    MsgResult := SendMessage(Edit.Handle, EM_GETSEL, Longint(@StartPos), Longint(@EndPos));
    Result := EndPos - StartPos;
    if (Result <= 0) and (MsgResult > 0) then
      Result := LongRec(MsgResult).Hi - LongRec(MsgResult).Lo;
  end;
end;

procedure THiddenPopupObject.SetEdit(const Value: TWinControl);
begin
  if FEdit <> Value then
  begin
    if FEdit <> nil then
      FEdit.RemoveFreeNotification(Self);
    FEdit := Value;
    if FEdit <> nil then
      FEdit.FreeNotification(Self);
  end;
end;

procedure THiddenPopupObject.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FEdit) then
    FEdit := nil;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  FreeAndNil(GlobalHiddenPopup);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.

