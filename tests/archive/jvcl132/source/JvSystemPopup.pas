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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvSystemPopup;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  JvTypes, JvComponent;

type
  TJvSystemPopup = class(TJvComponent)
  private
    FPopup: TPopupMenu;
    FPos: TPopupPosition;
    FForm: TForm;
    FOldWndProc: Pointer;
    FOnRefresh: TNotifyEvent;
    procedure NewWndProc(var Mesg: TMessage);
    procedure SetPopup(const Value: TPopupMenu);
    procedure SetPos(const Value: TPopupPosition);
    procedure MenuChanged(Sender: TObject; Source: TMenuItem; Rebuild: Boolean);
  protected
  public
    procedure Refresh;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Popup: TPopupMenu read FPopup write SetPopup;
    property Position: TPopupPosition read FPos write SetPos default ppNone;
    property OnRefreshed: TNotifyEvent read FOnRefresh write FOnRefresh;
  end;

implementation

{**************************************************}

constructor TJvSystemPopup.Create(AOwner: TComponent);
var
  ok: Boolean;
begin
  inherited;
  FPos := ppNone;
  FPopup := nil;
  ok := False;
  while not ok do
  begin
    if AOwner is TForm then
    begin
      FForm := AOwner as TForm;
      ok := True;
    end
    else
      AOwner := AOwner.Owner;
  end;
end;

{**************************************************}

destructor TJvSystemPopup.Destroy;
begin
  if not (csDesigning in ComponentState) then
    if not (csDestroying in Application.ComponentState) or (FPos = ppApplication) then
      Position := ppNone;
  inherited;
end;

{**************************************************}

procedure TJvSystemPopup.MenuChanged(Sender: TObject; Source: TMenuItem;
  Rebuild: Boolean);
begin
  case FPos of
    ppForm:
      GetSystemMenu(FForm.Handle, True);
    ppApplication:
      GetSystemMenu(Application.Handle, True);
  end;
  Refresh;
end;

{**************************************************}

procedure TJvSystemPopup.NewWndProc(var Mesg: TMessage);
var
  i: Integer;

  procedure ProcessSubs(Value: TMenuItem);
  var
    j: Integer;
  begin
    for j := 0 to Value.Count - 1 do
      if Value[j].Command = Cardinal(Mesg.WParam) then
        Value[j].Click
      else if Value[j].Count > 0 then
        ProcessSubs(Value[j]);
  end;

begin
  with Mesg do
  begin
    if Msg = WM_SYSCOMMAND then
      if FPopup <> nil then
        for i := 0 to FPopup.Items.Count - 1 do
          if FPopup.Items[i].Command = Cardinal(WParam) then
            FPopup.Items[i].Click
          else if FPopup.Items[i].Count > 0 then
            ProcessSubs(FPopup.ITems[i]);

    Result := CallWindowProc(FOldWndProc, FForm.Handle, Msg, WParam, LParam);
  end;
end;

{**************************************************}

procedure TJvSystemPopup.Refresh;
var
  h: THandle;
  i: Integer;
  men: TMenuItemInfo;

  procedure InsertSub(Parent: HMenu; Value: TMenuItem);
  var
    j: Integer;
  begin
    for j := 0 to Value.Count - 1 do
    begin
      if Value.Items[j].Visible then
      begin
        ZeroMemory(@men, SizeOf(men));

        men.cbSize := SizeOf(men);

        case Value.Items[j].Break of
          mbBarBreak:
            men.fType := MFT_MENUBARBREAK;
          mbBreak:
            men.fType := MFT_MENUBREAK;
          mbNone:
            men.fType := MFT_STRING;
        end;
        if Value.Items[j].Caption = '-' then
          men.fType := men.fType or MFT_SEPARATOR
        else
        begin
          men.cch := Length(Value.Items[j].Caption);
          men.dwTypeData := PChar(Value.Items[j].Caption);
        end;
        if Value.Items[j].RadioItem then
          men.fType := men.fType or MFT_RADIOCHECK;
        men.fMask := MIIM_CHECKMARKS or MIIM_DATA or MIIM_ID or MIIM_STATE or MIIM_SUBMENU or MIIM_TYPE;
        men.wID := Value.Items[j].Command;

        if Value.ITems[j].Count > 0 then
        begin
          men.hSubMenu := CreatePopupMenu;
          InsertMenuItem(Parent, DWORD(-1), True, men);
          InsertSub(men.hSubMenu, Value.Items[j]);
        end
        else
          InsertMenuItem(Parent, DWORD(-1), True, men);
      end;
    end;
  end;

begin
  if (csDesigning in ComponentState) or (FPos = ppNone) or (FPopup = nil) then
  begin
    if Assigned(FOnRefresh) then
      FOnRefresh(Self);
    Exit;
  end;

  if FPos = ppForm then
    h := FForm.Handle
  else
    h := Application.Handle;

  if FPopup.Items.Count > 0 then
  begin
    ZeroMemory(@men, SizeOf(men));
    men.cbSize := SizeOf(men);
    men.fMask := MIIM_CHECKMARKS or MIIM_DATA or MIIM_ID or MIIM_STATE or MIIM_SUBMENU or MIIM_TYPE;
    men.fType := MFT_SEPARATOR;
    InsertMenuItem(GetSystemMenu(h, False), DWORD(-1), True, men);
  end;

  for i := 0 to FPopup.Items.Count - 1 do
  begin
    if FPopup.Items[i].Visible then
    begin
      ZeroMemory(@men, SizeOf(men));

      men.cbSize := SizeOf(men);

      case FPopup.Items[i].Break of
        mbBarBreak:
          men.fType := MFT_MENUBARBREAK;
        mbBreak:
          men.fType := MFT_MENUBREAK;
        mbNone:
          men.fType := MFT_STRING;
      end;
      if FPopup.Items[i].Caption = '-' then
        men.fType := men.fType or MFT_SEPARATOR
      else
      begin
        men.cch := Length(FPopup.Items[i].Caption);
        men.dwTypeData := PChar(FPopup.Items[i].Caption);
      end;
      if FPopup.Items[i].RadioItem then
        men.fType := men.fType or MFT_RADIOCHECK;
      men.fMask := MIIM_CHECKMARKS or MIIM_DATA or MIIM_ID or MIIM_STATE or MIIM_SUBMENU or MIIM_TYPE;
      men.wID := FPopup.Items[i].Command;

      if FPopup.ITems[i].Count > 0 then
      begin
        men.hSubMenu := CreatePopupMenu;
        InsertMenuItem(GetSystemMenu(h, False), DWORD(-1), True, men);
        InsertSub(men.hSubMenu, FPopup.Items[i]);
      end
      else
        InsertMenuItem(GetSystemMenu(h, False), DWORD(-1), True, men);
    end;
  end;
  if Assigned(FOnRefresh) then
    FOnRefresh(Self);
end;

{**************************************************}

procedure TJvSystemPopup.SetPopup(const Value: TPopupMenu);
begin
  FPopup := Value;
  if FPopup <> nil then
    FPopup.OnChange := MenuChanged;
  case Fpos of
    ppForm:
      GetSystemMenu(FForm.Handle, True);
    ppApplication:
      GetSystemMenu(Application.Handle, True);
  end;
  Refresh;
end;

{**************************************************}

procedure TJvSystemPopup.SetPos(const Value: TPopupPosition);
var
  ptr: Pointer;
begin
  if FPos <> Value then
  begin
    //Update window function
    if not (csDesigning in ComponentState) then
    begin
      if (FForm <> nil) and (FOldWndProc <> nil) then
        case FPos of
          ppForm:
            SetWindowLong(FForm.Handle, GWL_WNDPROC, Longint(FOldWndProc));
          ppApplication:
            SetWindowLong(Application.Handle, GWL_WNDPROC, Longint(FOldWndProc));
        end;
      case Value of
        ppForm:
          begin
            FOldWndProc := Pointer(GetWindowLong(FForm.Handle, GWL_WNDPROC));
            ptr :=  {$IFDEF DELPHI6_UP}Classes.{$ENDIF}MakeObjectInstance(NewWndProc);
            SetWindowLong(FForm.Handle, GWL_WNDPROC, Longint(ptr));
          end;
        ppApplication:
          begin
            FOldWndProc := Pointer(GetWindowLong(Application.Handle, GWL_WNDPROC));
            ptr :=  {$IFDEF DELPHI6_UP}Classes.{$ENDIF}MakeObjectInstance(NewWndProc);
            SetWindowLong(Application.Handle, GWL_WNDPROC, Longint(ptr));
          end;
      end;
    end;

    //Reset popup
    case FPos of
      ppForm:
        GetSystemMenu(FForm.Handle, True);
      ppApplication:
        GetSystemMenu(Application.Handle, True);
    end;
    FPos := Value;
    Refresh;
  end;
end;

end.
