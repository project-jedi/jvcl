{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvControlPanel.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Peter Thörnqvist[peter3@peter4.com]
Remko Bonte [remkobonte@myrealbox.com]

Last Modified: 2002-07-06

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvControlPanel;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Menus, JvTypes, JvButton, JvDirectories, JvFunctions;

type
  TJvControlPanel = class(TJvButton)
  private
    FPopup: TPopupMenu;
    FDirs: TJvDirectories;
    FOnUrl: TOnLinkClick;
    FIMages: TImageList;
    procedure AddToPopup(Item: TMenuItem; Path: string);
    procedure UrlClick(Sender: TObject);
    procedure SetImages(const Value: TImageList);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh;
  published
    property Images: TImageList read FImages write SetImages;
    property OnLinkClick: TOnLinkClick read FOnUrl write FOnUrl;
  end;


implementation


{*******************************************************}

procedure TJvControlPanel.AddToPopup(Item: TMenuItem; Path: string);
var
  i: Integer;
  it: TMenuItem;
  S: TStringList;
  b: TBitmap;
begin
  S := TStringList.Create;
  try
    GetControlPanelApplets(Path, '*.cpl', S, Images);
    S.Sort;
    for i := 0 to S.Count - 1 do
    begin
      it := TMenuItem.Create(Self);
      it.Caption := S.Names[i];
      it.OnClick := UrlClick;
      it.Hint := S.Values[S.Names[i]];
      if Images <> nil then
        it.ImageIndex := integer(S.Objects[i])
      else
      begin
        b := TBitmap(S.Objects[i]);
        it.Bitmap.Assign(b);
        b.Free;
      end;
      item.Add(it);
      Application.ProcessMessages;
    end;
  finally
    S.Free;
  end;
end;

{*******************************************************}

procedure TJvControlPanel.Click;
var P: TPoint;
begin
  inherited;
  if Parent <> nil then
  begin
    P := Parent.ClientToScreen(Point(Left, Top + Height));
    FPopup.Popup(P.x, P.y);
  end;
end;

{*******************************************************}

constructor TJvControlPanel.Create(AOwner: TComponent);
begin
  inherited;
  FDirs := TJvDirectories.Create(Self);
  FPopup := TPopupMenu.Create(Self);
end;

{*******************************************************}

procedure TJvControlPanel.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Refresh;
end;

{*******************************************************}

destructor TJvControlPanel.Destroy;
var
  i: Integer;
begin
  FDirs.Free;
  if Images = nil then
    for i := 0 to FPopup.Items.Count - 1 do
      FPopup.Items[i].Bitmap.FreeImage;
  FPopup.Free;
  inherited;
end;

{*******************************************************}

procedure TJvControlPanel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil; // (p3) calls Refresh
end;

procedure TJvControlPanel.Refresh;
var
  st: string;
begin
  while FPopup.Items.Count > 0 do
    FPopup.Items.Delete(0);
  st := FDirs.SystemDirectory;
  if st[Length(st)] <> '\' then
    st := st + '\';
  FPopup.Images := Images;
  AddToPopup(TMenuItem(FPopup.Items), st);
  PopupMenu := FPopup;
end;

procedure TJvControlPanel.SetImages(const Value: TImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    Refresh;
  end;
end;

procedure TJvControlPanel.UrlClick(Sender: TObject);
begin
  if Assigned(FOnUrl) then
    FOnUrl(Self, (Sender as TMenuItem).Hint);
end;

end.

