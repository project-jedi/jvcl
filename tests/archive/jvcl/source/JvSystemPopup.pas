{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSystemPopup.PAS, released on 2001-02-28.

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

unit JvSystemPopup;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, JvTypes, JvComponent;

type
  TPositionInMenu = (pmTop, pmBottom);

  TJvSystemPopup = class(TJvComponent)
  private
    FPopup: TPopupMenu;
    FOwnerForm: TForm;
    FOldWndProc: TWndMethod;
    FIsHooked: Boolean;
    FPosition: TPopupPosition;
    FPositionInMenu: TPositionInMenu;

    procedure Hook;
    procedure UnHook;
    procedure ResetSystemMenu;
    function HandleWndProc(var Message: TMessage): Boolean;
    procedure WndProc(var Message: TMessage);
    procedure SetPopup(const Value: TPopupMenu);
    procedure MenuChanged(Sender: TObject; Source: TMenuItem; Rebuild: Boolean);
    procedure PopulateMenu;
    procedure SetPosition(const Value: TPopupPosition);
    procedure SetPositionInMenu(const Value: TPositionInMenu);
    function GetMenu: HMenu;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure Loaded; override;
  public
    procedure Refresh;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Popup: TPopupMenu read FPopup write SetPopup;
    property PositionInMenu: TPositionInMenu read FPositionInMenu write
      SetPositionInMenu default pmTop;
    property Position: TPopupPosition read FPosition write SetPosition default
      ppNone;
  end;

implementation

type
  TMenuItemProtectedAccess = class(TMenuItem);

  TMenuItemPrivateAccess = class(TComponent)
  private
    // FCaption *must* be included here: it's a hack to get the same offset for FHandle as in 
    // TMenuItem
    FCaption: string;
    FHandle: HMENU;
  end;

  {**************************************************}

constructor TJvSystemPopup.Create(AOwner: TComponent);
begin
  inherited;
  FPosition := ppNone;
  FPopup := nil;
  FPositionInMenu := pmTop;

  while Assigned(AOwner) and not (AOwner is TForm) do
    AOwner := AOwner.Owner;
  FOwnerForm := AOwner as TForm;
end;

{**************************************************}

destructor TJvSystemPopup.Destroy;
begin
  Position := ppNone;
  inherited;
end;

{**************************************************}

function TJvSystemPopup.GetMenu: HMenu;
begin
  { Return a handle to the copy of the window menu currently in use }

  Result := 0;
  case FPosition of
    ppNone:
      ;
    ppForm:
      if Assigned(FOwnerForm) then
        Result := GetSystemMenu(FOwnerForm.Handle, False);
    ppApplication:
      Result := GetSystemMenu(Application.Handle, False);
  end;
end;

{**************************************************}

function TJvSystemPopup.HandleWndProc(var Message: TMessage): Boolean;

  function Iterate(MenuItem: TMenuItem): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to MenuItem.Count - 1 do
      if MenuItem[i].Command = Cardinal(Message.WParam) then
      begin
        Result := True;
        MenuItem[i].Click;
      end
      else if MenuItem[i].Count > 0 then
        Result := Iterate(MenuItem[i]);
  end;

var
  SaveIndex: Integer;
  MenuItem: TMenuItem;
  Canvas: TCanvas;
  DC: HDC;
begin
  Result := False;
  case Message.Msg of
    WM_SYSCOMMAND:
      { Catch commands }
      if Assigned(FPopup) then
        Result := Iterate(FPopup.Items);
    WM_DRAWITEM:
      { Copied from Forms.pas }
      with PDrawItemStruct(Message.LParam)^ do
        if (CtlType = ODT_MENU) and Assigned(Popup) then
        begin
          MenuItem := Popup.FindItem(itemID, fkCommand);
          Result := MenuItem <> nil;
          if Result then
          begin
            Canvas := TControlCanvas.Create;
            with Canvas do
            try
              SaveIndex := SaveDC(hDC);
              try
                Handle := hDC;
                Font := Screen.MenuFont;
                Menus.DrawMenuItem(MenuItem, Canvas, rcItem,
                  TOwnerDrawState(LongRec(itemState).Lo));
              finally
                Handle := 0;
                RestoreDC(hDC, SaveIndex)
              end;
            finally
              Free;
            end;
          end;
        end;
    WM_MEASUREITEM:
      { Copied from Forms.pas }
      with PMeasureItemStruct(Message.LParam)^ do
        if (CtlType = ODT_MENU) and Assigned(Popup) then
        begin
          MenuItem := Popup.FindItem(itemID, fkCommand);
          Result := MenuItem <> nil;
          if Result then
          begin
            DC := GetWindowDC(Application.Handle);
            try
              Canvas := TControlCanvas.Create;
              with Canvas do
              try
                SaveIndex := SaveDC(DC);
                try
                  Handle := DC;
                  Font := Screen.MenuFont;
                  TMenuItemProtectedAccess(MenuItem).MeasureItem(Canvas,
                    Integer(itemWidth), Integer(itemHeight));
                finally
                  Handle := 0;
                  RestoreDC(DC, SaveIndex);
                end;
              finally
                Canvas.Free;
              end;
            finally
              ReleaseDC(Application.Handle, DC);
            end;
          end
        end;
  end;
end;

{**************************************************}

procedure TJvSystemPopup.Hook;
begin
  { Hook the application's window or the owner window of TJvSystemPopup }
  case FPosition of
    ppNone: ;
    ppForm:
      begin
        if not Assigned(FOwnerForm) then
          Exit;
        Assert(not FIsHooked, 'Already hooked');
        FOldWndProc := FOwnerForm.WindowProc;
        FOwnerForm.WindowProc := WndProc;
        FIsHooked := True;
      end;
    ppApplication:
      begin
        Assert(not FIsHooked, 'Already hooked');
        Application.HookMainWindow(HandleWndProc);
        FIsHooked := True;
      end;
  end;
end;

{**************************************************}

procedure TJvSystemPopup.Loaded;
begin
  inherited;
  Refresh;
end;

{**************************************************}

procedure TJvSystemPopup.MenuChanged(Sender: TObject; Source: TMenuItem;
  Rebuild: Boolean);
begin
  { Called if menu is loaded; menuitems are added/removed }
  Refresh;
end;

{**************************************************}

const
  RightToLeftMenuFlag = MFT_RIGHTORDER or MFT_RIGHTJUSTIFY;
  Checks: array[Boolean] of DWORD = (MF_UNCHECKED, MF_CHECKED);
  Enables: array[Boolean] of DWORD = (MF_DISABLED or MF_GRAYED, MF_ENABLED);
  Breaks: array[TMenuBreak] of DWORD = (0, MF_MENUBREAK, MF_MENUBARBREAK);
  Separators: array[Boolean] of DWORD = (MF_STRING, MF_SEPARATOR);

  { AppendMenuItemTo is copied from TMenuItem.AppendTo from Menus.pas }

function AppendMenuItemTo(Menu: HMENU; AMenuItem: TMenuItem;
  ARightToLeft: Boolean; InsertAt: Integer; var SubMenu: HMENU): Boolean;
const
  IBreaks: array[TMenuBreak] of DWORD = (MFT_STRING, MFT_MENUBREAK,
    MFT_MENUBARBREAK);
  IChecks: array[Boolean] of DWORD = (MFS_UNCHECKED, MFS_CHECKED);
  IDefaults: array[Boolean] of DWORD = (0, MFS_DEFAULT);
  IEnables: array[Boolean] of DWORD = (MFS_DISABLED or MFS_GRAYED, MFS_ENABLED);
  IRadios: array[Boolean] of DWORD = (MFT_STRING, MFT_RADIOCHECK);
  ISeparators: array[Boolean] of DWORD = (MFT_STRING, MFT_SEPARATOR);
  IRTL: array[Boolean] of DWORD = (0, RightToLeftMenuFlag);
  IOwnerDraw: array[Boolean] of DWORD = (MFT_STRING, MFT_OWNERDRAW);
var
  MenuItemInfo: TMenuItemInfo;
  Caption: string;
  NewFlags: Integer;
  IsOwnerDraw: Boolean;
  ParentMenu: TMenu;
begin
  Result := AMenuItem.Visible;
  if not Result then
    Exit;

  Caption := AMenuItem.Caption;
  if AMenuItem.Count > 0 then
  begin
    SubMenu := CreatePopupMenu;
    MenuItemInfo.hSubMenu := SubMenu;
  end
  else if (AMenuItem.ShortCut <> scNone) and ((AMenuItem.Parent = nil) or
    (AMenuItem.Parent.Parent <> nil) or not (AMenuItem.Parent.Owner is
    TMainMenu)) then
    Caption := Caption + #9 + ShortCutToText(AMenuItem.ShortCut);
  if Lo(GetVersion) >= 4 then
  begin
    MenuItemInfo.cbSize := 44; // Required for Windows 95
    MenuItemInfo.fMask := MIIM_CHECKMARKS or MIIM_DATA or MIIM_ID or
      MIIM_STATE or MIIM_SUBMENU or MIIM_TYPE;
    ParentMenu := AMenuItem.GetParentMenu;
    //      IsOwnerDraw := Assigned(ParentMenu) and ParentMenu.IsOwnerDraw or
    IsOwnerDraw := Assigned(ParentMenu) and
      (ParentMenu.OwnerDraw or (AMenuItem.GetImageList <> nil)) or
      Assigned(AMenuItem.Bitmap) and not AMenuItem.Bitmap.Empty;
    MenuItemInfo.fType := IRadios[AMenuItem.RadioItem] or
      IBreaks[AMenuItem.Break] or
      ISeparators[AMenuItem.Caption = cLineCaption] or IRTL[ARightToLeft] or
      IOwnerDraw[IsOwnerDraw];
    MenuItemInfo.fState := IChecks[AMenuItem.Checked] or
      IEnables[AMenuItem.Enabled]
      or IDefaults[AMenuItem.Default];
    MenuItemInfo.wID := AMenuItem.Command;
    MenuItemInfo.hSubMenu := 0;
    MenuItemInfo.hbmpChecked := 0;
    MenuItemInfo.hbmpUnchecked := 0;
    MenuItemInfo.dwTypeData := PChar(Caption);
    if AMenuItem.Count > 0 then
    begin
      MenuItemInfo.hSubMenu := SubMenu;
    end;
    InsertMenuItem(Menu, DWORD(InsertAt), True, MenuItemInfo);
  end
  else
  begin
    NewFlags := Breaks[AMenuItem.Break] or Checks[AMenuItem.Checked] or
      Enables[AMenuItem.Enabled] or
      Separators[AMenuItem.Caption = cLineCaption] or MF_BYPOSITION;
    if AMenuItem.Count > 0 then
      InsertMenu(Menu, DWORD(InsertAt), MF_POPUP or NewFlags,
        SubMenu, PChar(AMenuItem.Caption))
    else
      InsertMenu(Menu, DWORD(InsertAt), NewFlags, AMenuItem.Command,
        PChar(AMenuItem.Caption));
  end;
end;

{**************************************************}

procedure IterateMenu(AMenu: HMenu; AMenuItem: TMenuItem;
  ARightToLeft: Boolean; InsertAt: Integer);
var
  i: Integer;
  SubMenu: HMenu;
begin
  { We need a dirty hack to ensure that a menu item is changed when
    the properties of a TMenuItem are changed.
    TMenuItem uses it's handle to identify the menu item it's associated
    with. There is no clean way to set a TMenuItem's handle, so we use
    this trick to set a private var }

  TMenuItemPrivateAccess(AMenuItem).FHandle := AMenu;

  with AMenuItem do
    for i := 0 to Count - 1 do
    begin
      if AppendMenuItemTo(AMenu, Items[i], ARightToLeft, InsertAt, SubMenu) and
        (InsertAt >= 0) then
        Inc(InsertAt);

      if SubMenu > 0 then
        IterateMenu(SubMenu, Items[i], ARightToLeft, 0);
    end;
end;

{**************************************************}

procedure TJvSystemPopup.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FPopup) and (Operation = opRemove) then
    Popup := nil;
end;

{**************************************************}

procedure TJvSystemPopup.PopulateMenu;
var
  Menu: HMenu;
  MenuItemInfo: TMenuItemInfo;
  MenuRightToLeft: Boolean;
  InsertAt: Integer;
begin
  { Add all MenuItems to the systemmenu }
  if (ComponentState * [csDesigning, csLoading] <> []) or (FPosition = ppNone)
    or (FPopup = nil) then
    Exit;

  MenuRightToLeft := FPopup.IsRightToLeft;

  Menu := GetMenu;
  if Menu = 0 then
    Exit;

  if PositionInMenu = pmTop then
    InsertAt := 0
  else
    InsertAt := -1;

  if FPopup.Items.Count > 0 then
  begin
    { Add a seperator }
    ZeroMemory(@MenuItemInfo, SizeOf(MenuItemInfo));
    MenuItemInfo.cbSize := 44; //SizeOf(MenuItemInfo);
    MenuItemInfo.fMask := MIIM_CHECKMARKS or MIIM_DATA or MIIM_ID or MIIM_STATE
      or MIIM_SUBMENU or MIIM_TYPE;
    MenuItemInfo.fType := MFT_SEPARATOR;
    InsertMenuItem(Menu, DWORD(InsertAt), True, MenuItemInfo);
  end;

  TMenuItemPrivateAccess(FPopup.Items).FHandle := Menu;

  IterateMenu(Menu, FPopup.Items, MenuRightToLeft, InsertAt);
end;

{**************************************************}

procedure TJvSystemPopup.Refresh;
begin
  ResetSystemMenu;
  PopulateMenu;
end;

{**************************************************}

procedure TJvSystemPopup.ResetSystemMenu;
begin
  { Reset the window menu back to the default state. The previous window
    menu, if any, is destroyed. }

  if ComponentState * [csDesigning, csLoading] <> [] then
    Exit;

  case FPosition of
    ppNone:
      ;
    ppForm:
      if Assigned(FOwnerForm) and
        not (csDestroying in FOwnerForm.ComponentState) then
        GetSystemMenu(FOwnerForm.Handle, True);
    ppApplication:
      GetSystemMenu(Application.Handle, True);
  end;
end;

{**************************************************}

procedure TJvSystemPopup.SetPopup(const Value: TPopupMenu);
begin
  if Assigned(FPopup) then
    FPopup.OnChange := nil;

  FPopup := Value;

  if Assigned(FPopup) then
  begin
    FPopup.OnChange := MenuChanged;
    FPopup.FreeNotification(Self);
  end;

  if not (csLoading in ComponentState) then
    Refresh;
end;

{**************************************************}

procedure TJvSystemPopup.SetPosition(const Value: TPopupPosition);
begin
  if FPosition = Value then
    Exit;

  if csDesigning in ComponentState then
  begin
    FPosition := Value;
    Exit;
  end;

  UnHook;

  ResetSystemMenu;

  FPosition := Value;

  Hook;
  PopulateMenu;
end;

{**************************************************}

procedure TJvSystemPopup.SetPositionInMenu(const Value: TPositionInMenu);
begin
  FPositionInMenu := Value;
  if ComponentState * [csLoading, csDesigning] = [] then
    Refresh;
end;

{**************************************************}

procedure TJvSystemPopup.UnHook;
begin
  if not FIsHooked then
    Exit;

  case FPosition of
    ppNone: ;
    ppForm:
      begin
        if not Assigned(FOwnerForm) then
          Exit;
        FOwnerForm.WindowProc := FOldWndProc;
        FIsHooked := False;
      end;
    ppApplication:
      begin
        Application.UnhookMainWindow(HandleWndProc);
        FIsHooked := False;
      end;
  end;
end;

{**************************************************}

procedure TJvSystemPopup.WndProc(var Message: TMessage);
begin
  if not HandleWndProc(Message) then
    FOldWndProc(Message);
end;

end.

