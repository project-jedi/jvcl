{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvIDEZoom.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : Dephi IDE enhancement tool

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{$UNDEF COMPILER5_UP}

unit JvIDEZoom;

interface

uses
  Classes, SysUtils, Forms, Menus;

procedure RegisterZoom;

implementation

type
  {$IFDEF COMPILER5_UP}
  TJvEEditorZoom = class(TNotifierObject, IUnknown, IOTAKeyboardBinding)
  private
    procedure Zoom(Sender: TObject);
  public
    {$IFDEF COMPILER5_UP}
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
    {$ENDIF COMPILER5_UP}
  end;
  {$ELSE}
  TJvEEditorZoom = class(TObject)
  private
    procedure Zoom(Sender: TObject);
  public
  end;
  {$ENDIF COMPILER5_UP}

{$IFDEF COMPILER5_UP}
procedure TJvEEditorZoom.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([ShortCut(Ord('B'), [ssCtrl])], BufferListProc, nil);
end;
{$ENDIF COMPILER5_UP}

procedure Unregister;
var
  F: TForm;
  MenuItem: TMenuItem;
begin
  F := Application.FindComponent('AppBuilder') as TForm;
  if F <> nil then
  begin
    MenuItem := F.FindComponent('RAZoomEditor') as TMenuItem;
    if MenuItem <> nil then
      MenuItem.Free;
    MenuItem := F.FindComponent('RAZoomEditor2') as TMenuItem;
    if MenuItem <> nil then
      MenuItem.Free;
  end;
end;

procedure RegisterZoom;
var
  F: TForm;
  ViewsMenu, ViewNewEditorItem: TMenuItem;
  MenuItem: TMenuItem;
  Zoom: TJvEEditorZoom;
begin
  Unregister;
  Zoom := nil; {avoid warning}
  F := Application.FindComponent('AppBuilder') as TForm;
  if F <> nil then
  begin
    // ShowMessage('Found AppBuilder');
    ViewsMenu := F.FindComponent('ViewsMenu') as TMenuItem;
    if ViewsMenu = nil then
      Exit; {error}
    MenuItem := TMenuItem.Create(F);
    with MenuItem do
    begin
      Caption := 'Zoom Edit Window';
      ShortCut := Menus.ShortCut(ord('Z'), [ssAlt]);
      Name := 'RAZoomEditor';
      OnClick := Zoom.Zoom;
    end;
    ViewNewEditorItem := F.FindComponent('ViewNewEditorItem') as TMenuItem;
    if ViewNewEditorItem <> nil then
      ViewsMenu.Insert(ViewNewEditorItem.MenuIndex + 1, MenuItem)
    else
      ViewsMenu.Add(MenuItem);
    {Additional shortcut}
    MenuItem := TMenuItem.Create(F);
    with MenuItem do
    begin
      ShortCut := Menus.ShortCut(Ord('1'), [ssAlt]);
      Name := 'RAZoomEditor2';
      OnClick := Zoom.Zoom;
      Visible := false;
    end;
    ViewsMenu.Add(MenuItem);
  end;
  {$IFDEF COMPILER5_UP}
  (BorlandIDEServices as IOTAKeyBoardServices).AddKeyboardBinding(TJvEEditorZoom.Create);
  {$ENDIF COMPILER5_UP}
end;

procedure TJvEEditorZoom.Zoom(Sender: TObject);
var
  F: TForm;
  I: Integer;
 // MenuItem: TMenuItem;
begin
  F := Screen.ActiveForm;
  if not F.ClassNameIs('TEditWindow') then
  begin
    F := nil;
    for I := 0 to Screen.FormCount - 1 do
      if Screen.Forms[I].ClassNameIs('TEditWindow') then
      begin
        F := Screen.Forms[I];
        Break;
      end;
  end;
  if F <> nil then
    if F.WindowState <> wsMaximized then
      F.WindowState := wsMaximized
    else
      F.WindowState := wsNormal;
 { MenuItem := F.FindComponent('RAZoomEditor') as TMenuItem;
  if MenuItem <> nil then
    MenuItem.ShortCut := Menus.ShortCut(ord('Z'), [ssAlt]); }
end;

initialization

finalization
  Unregister;

end.

