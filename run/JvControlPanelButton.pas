{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvControlPanel.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Peter Th�rnqvist[peter3 at users dot sourceforge dot net]
Remko Bonte [remkobonte att myrealbox dott com]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvControlPanelButton;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes, Controls, Menus, ImgList,
  JvTypes, JvButton, JvComputerInfoEx;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvControlPanelButton = class(TJvCustomButton)
  private
    FPopup: TPopupMenu;
    FDirs: TJvSystemFolders;
    FOnLinkClick: TJvLinkClickEvent;
    FImages: TCustomImageList;
    procedure AddToPopup(Item: TMenuItem; Path: string);
    procedure SetImages(const Value: TCustomImageList);
  protected
    procedure DoLinkClick(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Click; override;
    procedure Refresh;
  published
    property Images: TCustomImageList read FImages write SetImages;
    property OnLinkClick: TJvLinkClickEvent read FOnLinkClick write FOnLinkClick;
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

uses
  Graphics,
  JvJVCLUtils;

constructor TJvControlPanelButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDirs := TJvSystemFolders.Create;
  FPopup := TPopupMenu.Create(Self);
end;

destructor TJvControlPanelButton.Destroy;
var
  I: Integer;
begin
  FDirs.Free;
  if Images = nil then
    for I := 0 to FPopup.Items.Count - 1 do
      FPopup.Items[I].Bitmap.FreeImage;
  FPopup.Free;
  inherited Destroy;
end;

// (rom) a strange place for doing this

procedure TJvControlPanelButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not (csDesigning in ComponentState) then
    Refresh;
end;

procedure TJvControlPanelButton.AddToPopup(Item: TMenuItem; Path: string);
var
  I: Integer;
  It: TMenuItem;
  S: TStringList;
  B: TBitmap;
begin
  S := TStringList.Create;
  try
    GetControlPanelApplets(Path, '*.cpl', S, Images);
    S.Sort;
    for I := 0 to S.Count - 1 do
    begin
      It := TMenuItem.Create(Self);
      It.Caption := S.Names[I];
      It.OnClick := DoLinkClick;
      It.Hint := S.Values[S.Names[I]];
      if Images <> nil then
        It.ImageIndex := Integer(S.Objects[I])
      else
      begin
        B := TBitmap(S.Objects[I]);
        It.Bitmap.Assign(B);
        B.Free;
      end;
      Item.Add(It);
      // (rom) seems of no use
      //Application.ProcessMessages;
    end;
  finally
    S.Free;
  end;
end;

procedure TJvControlPanelButton.Click;
var
  P: TPoint;
begin
  inherited Click;
  if Parent <> nil then
  begin
    P := Parent.ClientToScreen(Point(Left, Top + Height));
    FPopup.Popup(P.X, P.Y);
  end;
end;

procedure TJvControlPanelButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil; // (p3) calls Refresh
end;

procedure TJvControlPanelButton.Refresh;
var
  St: string;
begin
  while FPopup.Items.Count > 0 do
    FPopup.Items.Delete(0);
  St := FDirs.System;
  if St[Length(St)] <> '\' then
    St := St + '\';
  FPopup.Images := Images;
  AddToPopup(TMenuItem(FPopup.Items), St);
  PopupMenu := FPopup;
end;

procedure TJvControlPanelButton.SetImages(const Value: TCustomImageList);
begin
  if ReplaceComponentReference(Self, Value, TComponent(FImages)) then
    Refresh;
end;

procedure TJvControlPanelButton.DoLinkClick(Sender: TObject);
begin
  if Assigned(FOnLinkClick) then
    FOnLinkClick(Self, (Sender as TMenuItem).Hint);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
