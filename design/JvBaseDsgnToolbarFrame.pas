{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBaseDsgnToolbarFrame.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBaseDsgnToolbarFrame;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, Menus, ImgList, ToolWin, ComCtrls, ExtCtrls,
  JvBaseDsgnFrame;

type
  TfmeJvBaseToolbarDesign = class(TfmeJvBaseDesign)
    spToolbar: TSplitter;
    tbrToolbar: TToolBar;
    ilToolbar: TImageList;
    pmToolbar: TPopupMenu;
    miTextLabels: TMenuItem;
    aiToolbar: TActionList;
    aiTextLabels: TAction;
    aiShowToolbar: TAction;
    procedure aiTextLabelsExecute(Sender: TObject);
    procedure aiShowToolbarExecute(Sender: TObject);
    procedure spToolbarCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
  protected
    procedure Loaded; override;
  public
    function SettingsStored: Boolean; override;
    procedure StoreSettings; override;
    procedure RestoreSettings; override;
    procedure UpdateToolbarSeparators; dynamic;
    procedure EnableLargeButtons(Value: Boolean); dynamic;
    procedure ShowToolbar(Value: Boolean); dynamic;
  end;

implementation

uses
  Registry;

{$R *.dfm}

const
  cLargeButton = 'LargeButton';
  cToolbar = 'Toolbar';

procedure TfmeJvBaseToolbarDesign.Loaded;
begin
  inherited Loaded;
  UpdateToolbarSeparators;
end;

function TfmeJvBaseToolbarDesign.SettingsStored: Boolean;
begin
  Result := True;
end;

procedure TfmeJvBaseToolbarDesign.StoreSettings;
begin
  if RegKey <> '' then
    with TRegistry.Create do
    try
      LazyWrite := False;
      if OpenKey(RegKey, True) then
        try
          WriteBool(cLargeButton, aiTextLabels.Checked);
          WriteBool(cToolbar, aiShowToolbar.Checked);
        finally
          CloseKey;
        end;
    finally
      Free;
    end;
end;

procedure TfmeJvBaseToolbarDesign.RestoreSettings;
begin
  if RegKey <> '' then
    with TRegistry.Create do
    try
      if OpenKey(RegKey, False) then
        try
          EnableLargeButtons(not ValueExists(cLargeButton) or ReadBool(cLargeButton));
          ShowToolbar(not ValueExists(cToolbar) or ReadBool(cToolbar));
        finally
          CloseKey;
        end;
    finally
      Free;
    end;
end;

procedure TfmeJvBaseToolbarDesign.UpdateToolbarSeparators;
var
  LastVisibleSep: Integer;
  ButtonSinceLastSep: Boolean;
  I: Integer;
  CurItem: TControl;
begin
  LastVisibleSep := -1;
  ButtonSinceLastSep := False;
  for I := 0 to tbrToolbar.ButtonCount - 1 do
  begin
    CurItem := TControl(tbrToolbar.Buttons[I]);
    if (CurItem is TToolButton) and (TToolButton(CurItem).Style = tbsSeparator) then
    begin
      CurItem.Visible := ButtonSinceLastSep;
      if ButtonSinceLastSep then
        LastVisibleSep := I;
      ButtonSinceLastSep := False;
    end
    else
    if (CurItem is TToolButton) and (TToolButton(CurItem).Style <> tbsDivider) then
      ButtonSinceLastSep := ButtonSinceLastSep or CurItem.Visible
    else
    if not (CurItem is TToolButton) then
      ButtonSinceLastSep := ButtonSinceLastSep or CurItem.Visible;
  end;
  if (LastVisibleSep >= 0) and not ButtonSinceLastSep then
    tbrToolbar.Buttons[LastVisibleSep].Visible := False;
  { For some reason a divider may be drawn while it's invisible. Calling Invalidate didn't help but
    changing the ButtonWidth seems to work. Look into this issue; may have a different cause,
    possibly in this method. }
  tbrToolbar.ButtonWidth := 22;
end;

procedure TfmeJvBaseToolbarDesign.EnableLargeButtons(Value: Boolean);
begin
  tbrToolbar.ShowCaptions := Value;
  aiTextLabels.Checked := Value;
  if not Value then
  begin
    tbrToolbar.ButtonWidth := 22;
    tbrToolbar.ButtonHeight := 22;
  end;
end;

procedure TfmeJvBaseToolbarDesign.ShowToolbar(Value: Boolean);
begin
  aiShowToolbar.Checked := Value;
  Visible := Value;
  if Value then
  begin
    spToolbar.Parent := Parent;
    spToolbar.Top := Height;
  end
  else
    spToolbar.Parent := Self;
end;

procedure TfmeJvBaseToolbarDesign.aiTextLabelsExecute(Sender: TObject);
begin
  EnableLargeButtons(not aiTextLabels.Checked);
end;

procedure TfmeJvBaseToolbarDesign.aiShowToolbarExecute(Sender: TObject);
begin
  ShowToolbar(not aiShowToolbar.Checked);
end;

procedure TfmeJvBaseToolbarDesign.spToolbarCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  Accept := tbrToolbar.RowCount = 1;
  if Accept then
  begin
    if NewSize > 36 then
      NewSize := 44
    else
      NewSize := 30;
  end;
  EnableLargeButtons(NewSize = 44);
end;

end.