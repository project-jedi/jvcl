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
  Windows, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Menus,
  JvTypes, JvButton, JvDirectories, JvFunctions;

type
  TJvControlPanel = class(TJvCustomButton)
  private
    FPopup: TPopupMenu;
    FDirs: TJvDirectories;
    FOnLinkClick: TJvLinkClickEvent;
    FIMages: TImageList;
    procedure AddToPopup(Item: TMenuItem; Path: string);
    procedure SetImages(const Value: TImageList);
  protected
    procedure DoLinkClick(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh;
  published
    property Images: TImageList read FImages write SetImages;
    property OnLinkClick: TJvLinkClickEvent read FOnLinkClick write FOnLinkClick;
  end;

implementation

constructor TJvControlPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDirs := TJvDirectories.Create(Self);
  FPopup := TPopupMenu.Create(Self);
end;

destructor TJvControlPanel.Destroy;
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

procedure TJvControlPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not (csDesigning in ComponentState) then
    Refresh;
end;

procedure TJvControlPanel.AddToPopup(Item: TMenuItem; Path: string);
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

procedure TJvControlPanel.Click;
var
  P: TPoint;
begin
  inherited Click;
  if Parent <> nil then
  begin
    P := Parent.ClientToScreen(Point(Left, Top + Height));
    FPopup.Popup(P.x, P.y);
  end;
end;

procedure TJvControlPanel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil; // (p3) calls Refresh
end;

procedure TJvControlPanel.Refresh;
var
  St: string;
begin
  while FPopup.Items.Count > 0 do
    FPopup.Items.Delete(0);
  St := FDirs.SystemDirectory;
  if St[Length(St)] <> '\' then
    St := St + '\';
  FPopup.Images := Images;
  AddToPopup(TMenuItem(FPopup.Items), St);
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

procedure TJvControlPanel.DoLinkClick(Sender: TObject);
begin
  if Assigned(FOnLinkClick) then
    FOnLinkClick(Self, (Sender as TMenuItem).Hint);
end;

end.

