{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvOwnerDrawPageControl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, JVCLVer;

type
  TJvOwnerDrawPageControl = class(TPageControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
    { Private declarations }
    procedure WMLButtonDown(var msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure CMDialogKey(var msg: TWMKey); message CM_DIALOGKEY;
    procedure SetOwnerdraw(const Value: Boolean);
    function GetOwnerdraw: Boolean;
  protected
    { Protected declarations }
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); override;
  public
    { Public declarations }
    constructor Create(aOwner: TCOmponent); override;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;

    property Ownerdraw: Boolean read GetOwnerdraw write SetOwnerdraw default True;
  end;

implementation

uses CommCtrl;

{ TJvOwnerDrawPageControl }

procedure TJvOwnerDrawPageControl.CMDialogKey(var msg: TWMKEY);
var
  thistab, tab: TTabSheet;
  forward: Boolean;
begin
  if (msg.CharCode = Ord(#9)) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    thistab := ActivePage;
    forward := GetKeyState(VK_SHIFT) >= 0;
    tab := thistab;
    repeat
      tab := FindNextPage(tab, forward, true);
    until tab.Enabled or (tab = thistab);
    if tab <> thistab then
    begin
      if CanChange then
      begin
        ActivePage := tab;
        Change;
      end;
      Exit;
    end;
  end;
  inherited;
end;

constructor TJvOwnerDrawPageControl.Create(aOwner: TCOmponent);
begin
  inherited;
  OwnerDraw := True;
end;

procedure TJvOwnerDrawPageControl.DrawTab(TabIndex: Integer; const Rect: TRect;
  Active: Boolean);
var
  imageindex: Integer;
  r: TRect;
  S: string;
begin
  if not Pages[TabIndex].Enabled then
    Canvas.Font.Color := clGrayText;
  if Active then
    Canvas.Font.Style := [fsBold];
  if Assigned(OnDrawTab) then
    inherited
  else
  begin
    r := Rect;
    Canvas.Fillrect(r);
    imageindex := GetImageIndex(tabindex);
    if (imageindex >= 0) and Assigned(Images) then
    begin
      SaveDC(canvas.handle);
      images.Draw(Canvas, Rect.Left + 4, Rect.Top + 2,
        imageindex,
        Pages[TabIndex].enabled);
      // images.draw fouls the canvas colors if it draws
      // the image disabled, thus the SaveDC/RestoreDC
      RestoreDC(canvas.handle, -1);
      R.Left := R.Left + images.Width + 4;
    end;
    S := Pages[TabIndex].Caption;
    InflateRect(r, -2, -2);
    DrawText(Canvas.handle,
      PChar(S),
      Length(S),
      r,
      DT_SINGLELINE or DT_LEFT or DT_TOP);
  end;
end;

function TJvOwnerDrawPageControl.GetOwnerdraw: Boolean;
begin
  result := inherited OwnerDraw;
end;

procedure TJvOwnerDrawPageControl.SetOwnerdraw(const Value: Boolean);
begin
  inherited OwnerDraw := true;
end;

procedure TJvOwnerDrawPageControl.WMLButtonDown(var msg: TWMLButtonDown);
var
  hi: TTCHitTestInfo;
  tabindex: Integer;
begin
  if csDesigning in ComponentState then
  begin
    inherited;
    Exit;
  end;
  hi.pt.x := msg.XPos;
  hi.pt.y := msg.YPos;
  hi.flags := 0;
  tabindex := Perform(TCM_HITTEST, 0, longint(@hi));
  if (tabindex >= 0) and ((hi.flags and TCHT_ONITEM) <> 0) then
    if not Pages[tabindex].Enabled then
    begin
      msg.result := 0;
      Exit;
    end;
  inherited;
end;

end.

