{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvToolBar.PAS, released on 2001-02-28.

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

unit JvToolBar;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  JvTypes, JVCLVer, Menus, JvMenus;

type
  TJvToolBar = class;

  TJvToolButton = class (TToolButton)
  private
    function getMenuItem: TJvMenuItem;
    procedure setMenuItem(const Value: TJvMenuItem);
  public
    constructor Create(AOwner : TComponent); override; 
  published
    property MenuItem : TJvMenuItem read getMenuItem write setMenuItem;
  end;
   
  TJvToolBar = class(TToolBar)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FChangeLink : TJvMenuChangeLink;
    FHintColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOver: Boolean;
    FMenu : TMainMenu;
    FTempMenu : TJvPopupMenu;
    FButtonMenu : TJvMenuItem;
    FMenuButton : TJvToolButton;
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
//    function getButton(index: integer): TJvToolButton;
    procedure OnMenuChange(Sender: TJvMainMenu; Source: TJvMenuItem; Rebuild: Boolean);
    function GetMenu: TMainMenu;
    procedure SetMenu(const nMenu: TMainMenu);
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure ClearTempMenu;
  protected
    procedure AdjustSize; override;
  public
    constructor Create(AOwner: TComponent); override;
//    property Buttons[index : integer] : TJvToolButton read getButton stored false;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property Menu : TMainMenu read GetMenu write SetMenu; 
  end;

implementation

uses CommCtrl;

{ TJvToolBar }

constructor TJvToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clInfoBk;
  FOver := False;
  FChangeLink := TJvMenuChangeLink.Create;
  FChangeLink.OnChange := OnMenuChange;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

procedure TJvToolBar.MouseEnter(var Msg: TMessage);
begin
  FOver := True;
  FSaved := Application.HintColor;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  Application.HintColor := FHintColor;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvToolBar.MouseLeave(var Msg: TMessage);
begin
  Application.HintColor := FSaved;
  FOver := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvToolBar.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvToolBar.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

function TJvToolBar.GetMenu: TMainMenu;
begin
  Result := FMenu; //TToolbar(Self).Menu;
end;

procedure TJvToolBar.SetMenu(const nMenu: TMainMenu);
var
  I: Integer;
begin
  if FMenu = nMenu then exit;
  if csAcceptsControls in ControlStyle then
  begin
    ControlStyle := [csCaptureMouse, csClickEvents,
      csDoubleClicks, csMenuEvents, csSetCaption];
    RecreateWnd;
  end;
  ShowCaptions := True;
  if Assigned(FMenu) then
    for I := ButtonCount - 1 downto 0 do
      Buttons[I].Free;

  if Assigned(FMenu) then
    FMenu.RemoveFreeNotification(Self);
  FMenu := nMenu;
  if not Assigned(FMenu) then exit;
  FMenu.FreeNotification(Self);

  for I := ButtonCount to FMenu.Items.Count - 1 do
  begin
    with TJvToolButton.Create(Self) do
    try
      AutoSize := True;
      Grouped := True;
      Parent := Self;
      if FMenu is TJvMainMenu then
      begin
        Buttons[I].MenuItem := TJvMainMenu(FMenu).Items[I];
      end
      else
      begin
        TToolbar(Self).Buttons[I].MenuItem := FMenu.Items[I];
      end;
    except
      Free;
      raise;
    end;
  end;
  { Copy attributes from each menu item }
  for I := 0 to FMenu.Items.Count - 1 do
  begin
    if FMenu is TJvMainMenu then
    begin
      Buttons[I].MenuItem := TJvMainMenu(FMenu).Items[I];
    end
    else
    begin
      TToolbar(Self).Buttons[I].MenuItem := FMenu.Items[I];
    end;
  end;

  if nMenu is TJvMainMenu then
  begin
    // register a link with the menu to get informed when the
    // menu has changed
    TJvMainMenu(nMenu).RegisterChanges(FChangeLink);
  end;
end;

{function TJvToolBar.getButton(index: integer): TJvToolButton;
begin
  Result := TJvToolButton(TToolBar(Self).Buttons[index]);
end;}

procedure TJvToolBar.OnMenuChange(Sender: TJvMainMenu; Source: TJvMenuItem; Rebuild: Boolean);
begin
  if Sender = Menu then
  begin
    // if rebuild is necessary then
    if Rebuild then
    begin
      // force reloading menu by changing value twice
      Menu := nil;
      Menu := Sender;
    end;
  end;
end;

destructor TJvToolBar.Destroy;
begin
  if (Menu <> nil) and (Menu is TJvMainMenu) then
  begin
    TJvMainMenu(Menu).UnregisterChanges(FChangeLink);
  end;
  FChangeLink.Free;
  inherited;
end;

procedure TJvToolBar.AdjustSize;
var i : integer;
    totWidth : integer;
begin
  inherited;

  // if there is a menu and the toolbar is not wrapable,
  // update width according to sum of button widths
  if (Menu <> nil) and not Wrapable then
  begin
    totWidth := 0;
    for i := 0 to ButtonCount- 1 do
    begin
      totWidth := totWidth + Buttons[i].Width;
    end;
    Width := totWidth;
  end;
end;

procedure TJvToolBar.ClearTempMenu;
var
  I: Integer;
  Item: TMenuItem;
begin
  if (FButtonMenu <> nil) and (FMenuButton <> nil) and
    (FMenuButton.MenuItem <> nil) and (FTempMenu <> nil) then
  begin
    for I := FTempMenu.Items.Count - 1 downto 0 do
    begin
      Item := FTempMenu.Items[I];
      FTempMenu.Items.Delete(I);
      FButtonMenu.Insert(0, Item);
    end;
    FTempMenu.Free;
    FTempMenu := nil;
    FMenuButton := nil;
    FButtonMenu := nil;
  end;
end;

procedure TJvToolBar.CNNotify(var Message: TWMNotify);
var Button : TToolButton;
    ParentMenu: TJvMainMenu;
    I : integer;
    Item: TJvMenuItem;
begin
  if Menu is TJvMainMenu then
  begin
    with Message do
    begin
      case NMHdr^.code of
        TBN_DROPDOWN:
          with PNMToolBar(NMHdr)^ do
            { We can safely assume that a TBN_DROPDOWN message was generated by a
              TToolButton and not any TControl. }
            if Perform(TB_GETBUTTON, iItem, Longint(@tbButton)) <> 0 then
            begin
              Button := TToolButton(tbButton.dwData);
              if (Button <> nil) then
              begin
                Button.MenuItem.Click;
                ClearTempMenu;
                FTempMenu := TJvPopupMenu.Create(nil);
                FTempMenu.ShowCheckMarks := false;
                ParentMenu := TJvMainMenu(Button.MenuItem.GetParentMenu);
                if ParentMenu <> nil then
                begin
                  FTempMenu.BiDiMode := ParentMenu.BiDiMode;
                end;

                FTempMenu.HelpContext := Button.MenuItem.HelpContext;
                FTempMenu.TrackButton := tbLeftButton;
                Menu := TJvMainMenu(TJvToolButton(Button).MenuItem.GetParentMenu);
                if Menu <> nil then
                begin
                  FTempMenu.DisabledImages := TJvMainMenu(Menu).DisabledImages;
                  FTempMenu.Images := TJvMainMenu(Menu).Images;
                  FTempMenu.HotImages := TJvMainMenu(Menu).HotImages;
                end;
                FButtonMenu := TJvToolButton(Button).MenuItem;
                for I := FButtonMenu.Count - 1 downto 0 do
                begin
                  Item := TJvToolButton(Button).MenuItem.Items[I];
                  FButtonMenu.Delete(I);
                  FTempMenu.Items.Insert(0, Item);
                end;
                FMenuButton := TJvToolButton(Button);
                
                Button.DropdownMenu := FTempMenu;
                Button.CheckMenuDropdown;
              end;
            end;
        else inherited;
      end;
    end;
  end
  else
  begin
    inherited;
  end;
end;

{ TJvToolButton }

constructor TJvToolButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TJvToolButton.getMenuItem: TJvMenuItem;
begin
  Result := TJvMenuItem(TToolButton(Self).MenuItem);
end;

procedure TJvToolButton.setMenuItem(const Value: TJvMenuItem);
begin
  TToolButton(Self).MenuItem := Value;
end;


end.

