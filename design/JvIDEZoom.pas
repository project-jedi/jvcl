{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvIDEZoom.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Delphi IDE enhancement tool

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvIDEZoom;

{$I jvcl.inc}

interface

uses
  {$IFDEF VCL}
  Forms, Menus,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QForms, QMenus,
  {$ENDIF VisualCLX}
  Classes, SysUtils;

procedure RegisterZoom;

implementation

uses
  JvDsgnConsts;

const
  cAppBuilder = 'AppBuilder';
  cJvZoomEditor = 'JvZoomEditor';
  cJvZoomEditor2 = 'JvZoomEditor2';
  cViewsMenu = 'ViewsMenu';
  cViewNewEditorItem = 'ViewNewEditorItem';
  cTEditWindow = 'TEditWindow';

type
  // (rom) What is this?
  {$UNDEF COMPILER5_UP}
  {$IFDEF COMPILER5_UP}
  TJvEEditorZoom = class(TNotifierObject, IUnknown, IOTAKeyboardBinding)
  private
    procedure Zoom(Sender: TObject);
  public
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  end;
  {$ELSE}
  TJvEEditorZoom = class(TObject)
  private
    procedure Zoom(Sender: TObject);
  public
  end;
  {$ENDIF COMPILER5_UP}

procedure Unregister;
var
  F: TForm;
  MenuItem: TMenuItem;
begin
  F := Application.FindComponent(cAppBuilder) as TForm;
  if F <> nil then
  begin
    MenuItem := F.FindComponent(cJvZoomEditor) as TMenuItem;
    if MenuItem <> nil then
      MenuItem.Free;
    MenuItem := F.FindComponent(cJvZoomEditor2) as TMenuItem;
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
  Zoom := nil;
  F := Application.FindComponent(cAppBuilder) as TForm;
  if F <> nil then
  begin
    ViewsMenu := F.FindComponent(cViewsMenu) as TMenuItem;
    if ViewsMenu = nil then
      Exit; {error}
    MenuItem := TMenuItem.Create(F);
    with MenuItem do
    begin
      Caption := RsZoomEditWindow;
      ShortCut := Menus.ShortCut(Ord('Z'), [ssAlt]);
      Name := cJvZoomEditor;
      OnClick := Zoom.Zoom;
    end;
    ViewNewEditorItem := F.FindComponent(cViewNewEditorItem) as TMenuItem;
    if ViewNewEditorItem <> nil then
      ViewsMenu.Insert(ViewNewEditorItem.MenuIndex + 1, MenuItem)
    else
      ViewsMenu.Add(MenuItem);
    {Additional shortcut}
    MenuItem := TMenuItem.Create(F);
    with MenuItem do
    begin
      ShortCut := Menus.ShortCut(Ord('1'), [ssAlt]);
      Name := cJvZoomEditor2;
      OnClick := Zoom.Zoom;
      Visible := False;
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
  if not F.ClassNameIs(cTEditWindow) then
  begin
    F := nil;
    for I := 0 to Screen.FormCount - 1 do
      if Screen.Forms[I].ClassNameIs(cTEditWindow) then
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
 { MenuItem := F.FindComponent(cJvZoomEditor) as TMenuItem;
  if MenuItem <> nil then
    MenuItem.ShortCut := Menus.ShortCut(Ord('Z'), [ssAlt]); }
end;

{$IFDEF COMPILER5_UP}
procedure TJvEEditorZoom.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([ShortCut(Ord('B'), [ssCtrl])], BufferListProc, nil);
end;
{$ENDIF COMPILER5_UP}

initialization

finalization
  Unregister;

end.

