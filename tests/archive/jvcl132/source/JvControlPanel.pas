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

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvControlPanel;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Menus, Cpl, ShellApi,
  JvTypes, JvButton, JvDirectories, JvFunctions;

type
  TJvControlPanel = class(TJvButton)
  private
    FPopup: TPopupMenu;
    FDirs: TJvDirectories;
    FOnUrl: TOnLinkClick;
    procedure AddToPopup(Item: TMenuItem; Path: string);
    procedure UrlClick(Sender: TObject);
  public
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnLinkClick: TOnLinkClick read FOnUrl write FOnUrl;
  end;

implementation

resourcestring
  RC_CplAddress = 'CPlApplet';

  {*******************************************************}

function GetNameCpl(Path: string): string;
var
  h: THandle;
  CplApplet: TCplApplet;
  NewCplInfo: TNewCplInfo;
begin
  // (rom) simplified/fixed
  Result := '';
  h := LoadLibrary(PChar(Path));
  if h <> 0 then
  begin
    @CplApplet := GetProcAddress(h, PChar(RC_CplAddress));
    if @CplApplet <> nil then
    begin
      NewCplInfo.szName[0] := #0;
      CplApplet(0, CPL_NEWINQUIRE, 0, Longint(@NewCplInfo));
      Result := NewCplInfo.szName;
    end;
    FreeLibrary(h);
  end;
end;

{*******************************************************}

procedure TJvControlPanel.AddToPopup(Item: TMenuItem; Path: string);
var
  t: TSearchRec;
  res, i: Integer;
  it: TMenuItem;
  ts: TStringList;
  st: string;
  w: Word;
begin
  ts := TStringList.Create;
  res := FindFirst(Path + '*.cpl', faAnyFile, t);
  while res = 0 do
  begin
    if (t.Name <> '.') and (t.Name <> '..') then
    begin
      st := GetNameCpl(Path + t.Name);
      if st = '' then
      begin
        st := t.Name;
        i := Length(st);
        while (i > 0) and (st[i] <> '.') do
          Dec(i);
        st := Copy(st, 1, i - 1);
      end;
      ts.Add(st + '%' + t.Name);
    end;
    res := FindNext(t);
  end;
  FindClose(t);
  ts.Sort;

  for res := 0 to ts.Count - 1 do
  begin
    it := TMenuItem.Create(Self);
    it.Caption := Copy(ts[res], 1, Pos('%', ts[res]) - 1);
    it.OnClick := UrlClick;
    it.Hint := Path + Copy(ts[res], Pos('%', ts[res]) + 1, Length(ts[res]));
    ;
    w := 0;
    it.Bitmap.Assign(IconToBitmap(ExtractAssociatedIcon(Application.Handle, PChar(it.Hint), w)));
    it.Bitmap.TransparentMode := tmAuto;
    item.Add(it);
    Application.ProcessMessages;
  end;
  ts.Free;
end;

{*******************************************************}

procedure TJvControlPanel.Click;
var P: TPoint;
begin
  inherited;
  if Parent <> nil then
  begin
    P := ClientToScreen(Point(Left, Top + Height));
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
var
  st: string;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    while FPopup.Items.Count > 0 do
      FPopup.Items.Delete(0);
    st := FDirs.SystemDirectory;
    if st[Length(st)] <> '\' then
      st := st + '\';
    AddToPopup(TMenuItem(FPopup.Items), st);
    PopupMenu := FPopup;
  end;
end;

{*******************************************************}

destructor TJvControlPanel.Destroy;
var
  i: Integer;
begin
  FDirs.Free;
  for i := 0 to FPopup.Items.Count - 1 do
    Fpopup.Items[i].Bitmap.FreeImage;
  FPopup.Free;
  inherited;
end;

{*******************************************************}

procedure TJvControlPanel.UrlClick(Sender: TObject);
begin
  if Assigned(FOnUrl) then
    FOnUrl(Self, (Sender as TMenuItem).Hint);
end;

end.

