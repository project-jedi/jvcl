{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvToolBar.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
  Michael Beck [mbeck att bigfoot dott com].
  Olivier Sannier [obones att altern dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvToolBar;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, CommCtrl, SysUtils, Classes, Graphics, Controls,
  Forms, ComCtrls, Menus,
  JvMenus, JvExComCtrls;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvToolBar = class(TJvExToolBar)
  private
    FChangeLink: TJvMenuChangeLink;
    FTempMenu: TJvPopupMenu;
    FButtonMenu: TMenuItem;
    FMenuShowingCount: Integer;
    procedure ClearTempMenu;
    function GetMenu: TMainMenu;
    procedure SetMenu(const Value: TMainMenu);
    procedure MenuChange(Sender: TJvMainMenu; Source: TMenuItem; Rebuild: Boolean);
    procedure CNNotify(var Msg: TWMNotify); message CN_NOTIFY;
    procedure CNDropDownClosed(var Msg: TMessage); message CN_DROPDOWNCLOSED;
  protected
    procedure AdjustSize; override;
  {$IFDEF COMPILER12_UP}
  public
  {$ENDIF COMPILER12_UP}
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property HintColor;
    property Menu: TMainMenu read GetMenu write SetMenu;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

constructor TJvToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChangeLink := TJvMenuChangeLink.Create;
  FChangeLink.OnChange := MenuChange;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FMenuShowingCount := 0;
end;

destructor TJvToolBar.Destroy;
begin
  if (Menu <> nil) and (Menu is TJvMainMenu) then
    TJvMainMenu(Menu).UnregisterChanges(FChangeLink);
  FChangeLink.Free;
  inherited Destroy;
end;

function TJvToolBar.GetMenu: TMainMenu;
begin
  Result := inherited Menu;
end;

procedure TJvToolBar.SetMenu(const Value: TMainMenu);
begin
  // if trying to set the same menu, do nothing
  if Menu = Value then
    Exit;

  if Assigned(Menu) and (Menu is TJvMainMenu) then
    // if the current menu is a TJvMainMenu, we must
    // unregister us from being told the changes
    TJvMainMenu(Menu).UnregisterChanges(FChangeLink);

  if Value is TJvMainMenu then
    // if the new menu is a TJvMainMenu then we register a link
    // with the menu to get informed when it has changed
    TJvMainMenu(Value).RegisterChanges(FChangeLink);

  // and we set the inherited value, so that the inherited
  // methods can deal with the menu too, the most obvious
  // one being the creation of the required TToolButton
  inherited Menu := Value;
end;

procedure TJvToolBar.MenuChange(Sender: TJvMainMenu; Source: TMenuItem; Rebuild: Boolean);
var
  VisibleMenuItemsCount, VisibleButtonsCount: Integer;
  I: Integer;
begin
  if Sender = Menu then
  begin
    // Compute our own value for rebuild, as the value passed
    // to us is not correct (see TJvMenuChangeLink for details)
    // We rebuild if the number of visible items in the menu is different
    // from the number of buttons visible in the toolbar.
    VisibleMenuItemsCount := 0;
    for I := 0 to Menu.Items.Count-1 do
      if Menu.Items[i].Visible then
        Inc(VisibleMenuItemsCount);
    VisibleButtonsCount := 0;
    for I := 0 to ButtonCount-1 do
      if Buttons[i].Visible then
        Inc(VisibleButtonsCount);
    Rebuild := VisibleMenuItemsCount <> VisibleButtonsCount;

    // if rebuild is necessary then
    if Rebuild then
    begin
      // force reloading menu by changing value twice
      // this is the only way of doing it as the creation of
      // the TToolButton is done in the original SetMenu in
      // TToolbar and this procedure is private
      Menu := nil;
      Menu := Sender;
    end;
  end;
end;

procedure TJvToolBar.AdjustSize;
var
  I: Integer;
  TotWidth: Integer;
begin
  inherited AdjustSize;

  // if there is a menu and the toolbar is not wrapable,
  // update width according to sum of button widths
  if (Menu <> nil) and not Wrapable then
  begin
    TotWidth := 0;
    for I := 0 to ButtonCount - 1 do
      TotWidth := TotWidth + Buttons[I].Width;
    Width := TotWidth;
  end;
end;

procedure TJvToolBar.ClearTempMenu;
var
  I: Integer;
  Item: TMenuItem;
begin
  if (FButtonMenu <> nil) and (FTempMenu <> nil) then
  begin
    for I := FTempMenu.Items.Count - 1 downto 0 do
    begin
      Item := FTempMenu.Items[I];
      FTempMenu.Items.Delete(I);
      FButtonMenu.Insert(0, Item);
    end;
    FTempMenu.Free;
    FTempMenu := nil;
    FButtonMenu := nil;
  end;
end;

procedure TJvToolBar.CNNotify(var Msg: TWMNotify);
var
  Button: TToolButton;
  JvParentMenu: TJvMainMenu;
  Menu: TMenu;
  I: Integer;
  Item: TMenuItem;
begin
  // we process the WM_NOTIFY message ourselves to be able to
  // display a dropdown JvMenu instead of a regular one.
  // However, we do that only if the menu is a TJvMainMenu and
  // if the code in WM_NOTIFY is TBN_DROPDOWN. Anything else
  // is given back to the inherited method.
  // The code is mostly inspired from the Delphi 6 VCL source code,
  // the major change being the creation of a TJvPopupMenu
  // instead of a TPopupMenu.
  with Msg do
  begin
    case NMHdr^.code of
      TBN_DROPDOWN:
        with PNMToolBar(NMHdr)^ do
          { We can safely assume that a TBN_DROPDOWN message was generated by a
            TToolButton and not any TControl. }
          if Perform(TB_GETBUTTON, iItem, LPARAM(@tbButton)) <> 0 then
          begin
            Button := TToolButton(tbButton.dwData);
            if Button <> nil then
            begin
              Menu := nil;
              if Button.MenuItem <> nil then
                Menu := Button.MenuItem.GetParentMenu;
              if Menu is TJvMainMenu then
              begin
                JvParentMenu := Button.MenuItem.GetParentMenu as TJvMainMenu;
                Button.MenuItem.Click;
                ClearTempMenu;
                FTempMenu := TJvPopupMenu.Create(nil);
                if JvParentMenu <> nil then
                  FTempMenu.BiDiMode := JvParentMenu.BiDiMode;
                FTempMenu.HelpContext := Button.MenuItem.HelpContext;
                FTempMenu.TrackButton := tbLeftButton;
                Menu := Button.MenuItem.GetParentMenu;
                if Menu <> nil then
                  FTempMenu.Assign(JvParentMenu);
                FButtonMenu := Button.MenuItem;
                for I := FButtonMenu.Count - 1 downto 0 do
                begin
                  Item := FButtonMenu.Items[I];
                  FButtonMenu.Delete(I);
                  FTempMenu.Items.Insert(0, Item);
                end;

                Button.DropdownMenu := FTempMenu;
                // for some reason, while the menu is showing,
                // it is possible that a second message comes
                // up and asks for the menu to show up.
                // so we keep track of that fact, and only when
                // the count comes back to 0, we hide the menu
                // in the CN_DROPDOWNCLOSED handler
                Inc(FMenuShowingCount);
                // show the temporary popup menu
                Button.CheckMenuDropdown;
              end
              else
                inherited;
            end;
          end;
    else
      inherited;
    end;
  end;
end;

procedure TJvToolBar.CNDropDownClosed(var Msg: TMessage);
begin
  if FMenuShowingCount = 1 then
    ClearTempMenu;
  Dec(FMenuShowingCount);
  inherited;
end;

procedure TJvToolBar.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  // This is required by v5 VCL so that it doesn't save the buttons
  // created because of the menu property. This is redundant
  // under v6 VCL because it already does that check.
  if not Assigned(Menu) then
    inherited GetChildren(Proc, Root);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
