{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFixedEditPopUp.PAS, released 2003-03-01.

The Initial Developer of the Original Code is Peter Thornqvist (peter3@peter3.com)
Portions created by Peter Thornqvist are Copyright (C) 2002 Peter Thornqvist .
All Rights Reserved.

Contributor(s):
Steve Magruder
Remko Bonte

Last Modified: 2003-07-14

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  A unit that can be used to replace the default system popup menu for edit controls. The problem with
  MS implementation is that the Paste command is enabled even when the edit is set to read-only

  By overriding TCustomEdit.GetPopupMenu (virtual), you can return an instance of this popup menu
  instead of the default, i.e:
  interface:

    function GetPopupMenu:TPopupMenu;override;

  implementation:

    function TMyEdit.GetPopupMenu:TPopupMenu;
    begin
      Result := inherited GetPopupMenu;
      if Result = nil then // user has not assigned his own popup menu, so use fixed default
        Result := FixedDefaultEditPopUp(self);
    end;

  The popup is constructed as a singelton shared between all edit controls using it, so it is
  as resource friendly as we could make it and you should NOT free it after use. The popup is
  not created until first use, so if you don't use it, it doesn't take any resources.

  The popup automatically handles cut, copy, paste, select all, clear and undo events and it's aware
  of and can also handle the ClipboardCommands property in some JVCL edits. Menu items
  are automatically enabled / disabled according to the current state of the edit.

  The popup is "self-translating" based on Windows locale. If you want to
  use resourcestrings and supply your own translations, call FixedDefaultEditPopUseResourceString(true);
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



-----------------------------------------------------------------------------}

unit JvFixedEditPopUp;

interface
uses
  Controls, Menus;

// Returns a popup menu with the standard actions associated with edit controls (Undo, Cut, Copy, Paste, Delete, Select All).
// The actions are handled autmatically by sending messages (WM_COPY, WM_CUT etc) to the control
function FixedDefaultEditPopUp(AEdit:TWinControl):TPopUpMenu;
// Call with Value set to true to use the internal resourcestrings instead of those
// provided by Windows. These strings can subsequently be translated using the ITE.
// By default, the Windows provided strings are used.
procedure FixedDefaultEditPopUseResourceString(Value:boolean);

implementation
uses
  Windows, SysUtils, Classes, Messages, JvComponent, TypInfo;

resourcestring
  SDefaultPopUpUndo = '&Undo';
  SDefaultPopUpCut  = 'Cu&t';
  SDefaultPopUpCopy = '&Copy';
  SDefaultPopUpPaste = '&Paste';
  SDefaultPopUpDelete = '&Delete';
  SDefaultPopUpSelAll = 'Select &All';

{
  SWEDISH:
  SDefaultPopUpUndo = '&Ångra';
  SDefaultPopUpCut  = '&Klipp ut';
  SDefaultPopUpCopy = 'K&opiera';
  SDefaultPopUpPaste = 'Kl&istra in';
  SDefaultPopUpDelete = '&Ta bort';
  SDefaultPopUpSelAll = '&Markera allt';

  --add other languages here--
}
type
  THiddenPopupObject = class(TComponent)
  private
    FEdit: TWinControl;
    FPopUpMenu:TPopUpMenu;
    procedure GetDefaultMenuCaptions;
    procedure DoSelectAll(Sender:TObject);
    procedure DoUndo(Sender:TObject);
    procedure DoDelete(Sender:TObject);
    procedure DoPaste(Sender:TObject);
    procedure DoCut(Sender:TObject);
    procedure DoCopy(Sender:TObject);
    function CanUndo:boolean;
    function ReadOnly:boolean;
    function GetTextLen:integer;
    function SelLength:integer;
    function GetPopUpMenu: TPopUpMenu;
    procedure SetEdit(const Value: TWinControl);
    function GetClipboardCommands:TJvClipboardCommands;
  public
    property Edit:TWinControl read FEdit write SetEdit;
    property PopUpMenu:TPopUpMenu read GetPopUpMenu;
  end;

var
  FHiddenPopup:THiddenPopupObject = nil;
  FUseResourceStrings:boolean = false;

function FixedDefaultEditPopUp(AEdit:TWinControl):TPopUpMenu;
begin
  if FHiddenPopup = nil then
    FHiddenPopup := THiddenPopupObject.Create(nil);
  FHiddenPopup.Edit := AEdit;
  Result := FHiddenPopup.PopUpMenu;
end;

procedure FixedDefaultEditPopUseResourceString(Value:boolean);
begin
  FUseResourceStrings := Value;
end;

{ THiddenPopupObject }

function THiddenPopupObject.CanUndo: boolean;
begin
  if (Edit <> nil) and Edit.HandleAllocated then
    Result := SendMessage(Edit.Handle, EM_CANUNDO, 0, 0) <> 0
  else
    Result := false;
end;

procedure THiddenPopupObject.DoCopy(Sender: TObject);
begin
  if Assigned(Edit) and Edit.HandleAllocated then
    Edit.Perform(WM_COPY,0,0);
end;

procedure THiddenPopupObject.DoCut(Sender: TObject);
begin
  if Assigned(Edit) and Edit.HandleAllocated then
    Edit.Perform(WM_CUT, 0, 0);
end;

procedure THiddenPopupObject.DoDelete(Sender: TObject);
begin
  if Assigned(Edit) and Edit.HandleAllocated then
    Edit.Perform(WM_CLEAR, 0, 0);
end;

procedure THiddenPopupObject.DoPaste(Sender: TObject);
begin
  if Assigned(Edit) and Edit.HandleAllocated then
    Edit.Perform(WM_PASTE,0,0);
end;

procedure THiddenPopupObject.DoSelectAll(Sender: TObject);
begin
  if Assigned(Edit) and Edit.HandleAllocated then
    Edit.Perform(EM_SETSEL, 0, -1);
end;

procedure THiddenPopupObject.DoUndo(Sender: TObject);
begin
  if Assigned(Edit) and Edit.HandleAllocated then
    Edit.Perform(WM_UNDO, 0, 0);
end;

type
  TIntegerSet = set of 0..sizeof(integer) * 8 - 1;
  
function THiddenPopupObject.GetClipboardCommands: TJvClipboardCommands;
var Value:TIntegerSet;i:integer;
begin
  if IsPublishedProp(Edit,'ClipboardCommands') then
  begin
    Result := [];
    // does it really have to be this complicated ?!
    integer(Value) := GetOrdProp(Edit,'ClipboardCommands');
    for i := 0 to sizeof(integer) * 8 - 1 do
      if i in Value then
        Include(Result,TJvClipboardCommand(i));
  end
  else
    Result := [caCopy, caCut, caPaste, caUndo];
end;

procedure THiddenPopupObject.GetDefaultMenuCaptions;
var H:HMODULE;
  hMenu,hSubMenu:THandle;
  buf:array[0..255] of char;
begin
  // get the translated captions from Windows' own default popup:
  H := GetModuleHandle('user32.dll');
  hMenu := LoadMenu(H,MakeIntResource(1));
  if hMenu = 0 then Exit;
  try
    hSubMenu := GetSubMenu(hMenu,0);
    if hSubMenu = 0 then Exit;

    if GetMenuString(hSubMenu,WM_UNDO,buf,sizeof(buf),MF_BYCOMMAND) <> 0 then
      FPopUpMenu.Items[0].Caption := buf;

    if GetMenuString(hSubMenu,WM_CUT,buf,sizeof(buf),MF_BYCOMMAND) <> 0 then
      FPopUpMenu.Items[2].Caption := buf;
    if GetMenuString(hSubMenu,WM_COPY,buf,sizeof(buf),MF_BYCOMMAND) <> 0 then
      FPopUpMenu.Items[3].Caption := buf;
    if GetMenuString(hSubMenu,WM_PASTE,buf,sizeof(buf),MF_BYCOMMAND) <> 0 then
      FPopUpMenu.Items[4].Caption := buf;
    if GetMenuString(hSubMenu,WM_CLEAR,buf,sizeof(buf),MF_BYCOMMAND) <> 0 then
      FPopUpMenu.Items[5].Caption := buf;

    if GetMenuString(hSubMenu,EM_SETSEL,buf,sizeof(buf),MF_BYCOMMAND) <> 0 then
      FPopUpMenu.Items[7].Caption := buf;

  finally
    DestroyMenu(hMenu);
  end;
end;

function THiddenPopupObject.GetPopUpMenu: TPopUpMenu;
var m:TMenuItem;
    cc:TJvClipboardCommands;
begin
  if FPopUpMenu = nil then
  begin
    FPopUpMenu := TPopUpMenu.Create(self);
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
    m := TMenuItem.Create(self);
    m.Caption := SDefaultPopUpUndo;
    m.OnClick := DoUndo;
    FPopUpMenu.Items.Add(m);

    m := TMenuItem.Create(self);
    m.Caption := '-';
    FPopUpMenu.Items.Add(m);

    m := TMenuItem.Create(self);
    m.Caption := SDefaultPopUpCut;
    m.OnClick := DoCut;
    FPopUpMenu.Items.Add(m);

    m := TMenuItem.Create(self);
    m.Caption := SDefaultPopUpCopy;
    m.OnClick := DoCopy;
    FPopUpMenu.Items.Add(m);

    m := TMenuItem.Create(self);
    m.Caption := SDefaultPopUpPaste;
    m.OnClick := DoPaste;
    FPopUpMenu.Items.Add(m);

    m := TMenuItem.Create(self);
    m.Caption := SDefaultPopUpDelete;
    m.OnClick := DoDelete;
    FPopUpMenu.Items.Add(m);

    m := TMenuItem.Create(self);
    m.Caption := '-';
    FPopUpMenu.Items.Add(m);
    m := TMenuItem.Create(self);
    m.Caption := SDefaultPopUpSelAll;
    m.OnClick := DoSelectAll;
    FPopUpMenu.Items.Add(m);
    if not FUseResourceStrings then
      GetDefaultMenuCaptions;
  end;
  if (Edit <> nil) and Edit.HandleAllocated then
  begin
    cc := GetClipboardCommands;

    FPopUpMenu.PopupComponent := Edit;
    // undo
    FPopUpMenu.Items[0].Enabled := CanUndo and (caUndo in cc);
    // cut
    FPopUpMenu.Items[2].Enabled := (SelLength > 0) and not ReadOnly and (caCut in cc);
    // copy
    FPopUpMenu.Items[3].Enabled := (SelLength > 0) and (caCopy in cc);
    // paste
    FPopUpMenu.Items[4].Enabled := not ReadOnly and (caPaste in cc);
    // delete
    FPopUpMenu.Items[5].Enabled := (SelLength > 0) and not ReadOnly { and (caCut in cc)};
    // select all
    FPopUpMenu.Items[7].Enabled := (GetTextLen > 0) and (SelLength <> GetTextLen);
  end;

  Result := FPopUpMenu;
end;

function THiddenPopupObject.GetTextLen: integer;
begin
  if (Edit <> nil) and Edit.HandleAllocated then
    Result := Edit.GetTextLen
  else
    Result := 0;
end;

function THiddenPopupObject.ReadOnly: boolean;
begin
  if (Edit <> nil) and Edit.HandleAllocated then
    Result := GetWindowLong(Edit.Handle, GWL_STYLE) and ES_READONLY = ES_READONLY
  else
    Result := false;
end;

type
  TSelection = record
    StartPos, EndPos: Integer;
  end;

function THiddenPopupObject.SelLength: integer;
var
  Selection: TSelection;
  MsgResult:Longint;
begin
  Result := 0;
  if (Edit <> nil) and Edit.HandleAllocated then
  begin
    Selection.StartPos := 0;
    Selection.EndPos := 0;
    MsgResult := SendMessage(Edit.Handle, EM_GETSEL, Longint(@Selection.StartPos), Longint(@Selection.EndPos));
//    MsgResult := SendMessage(Edit.Handle, EM_GETSEL, 0, 0); // (p3) for testing only
    Result := Selection.EndPos - Selection.StartPos;
    if (Result <= 0) and (MsgResult > 0) then
      Result := LongRec(MsgResult).Hi - LongRec(MsgResult).Lo;
  end;
end;

procedure THiddenPopupObject.SetEdit(const Value: TWinControl);
begin
  if FEdit <> Value then
  begin
    if FEdit <> nil then
      FEdit.RemoveFreeNotification(self);
    FEdit := Value;
    if FEdit <> nil then
      FEdit.FreeNotification(self);
  end;
end;

initialization

finalization
  FHiddenPopup.Free;
end.
