{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTrayBar.PAS, released on 2001-02-28.

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

unit JvTrayBar;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvTypes, JvComponent;

type
  TJvTraybar = class(TJvComponent)
  private
    FHWnd: HWND;
  protected
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
  published
    procedure HideTraybar;
    procedure ShowTraybar;
    procedure ShowStartButton;
    procedure HideStartButton;
  end;

implementation

resourcestring
  RC_ShellName = 'Shell_TrayWnd';

  {***************************************************}

constructor TJvTraybar.Create(AOwner: TComponent);
begin
  inherited;
  FHWnd := FindWindow(PChar(RC_ShellName), nil);
end;

{***************************************************}

destructor TJvTraybar.Destroy;
begin
  ShowTrayBar;
  ShowStartButton;
  inherited;
end;

{***************************************************}

procedure TJvTraybar.HideTraybar;
begin
  ShowWindow(FHWnd, SW_HIDE);
end;

{***************************************************}

procedure TJvTraybar.ShowTraybar;
begin
  ShowWindow(FHWnd, SW_SHOW);
end;

{***************************************************}

procedure HideStartBtn(Visible: Boolean);
var
  Tray, Child: HWND;
  C: array[0..127] of Char;
  S: string;
begin
  Tray := FindWindow(PChar(RC_ShellName), nil);
  Child := GetWindow(Tray, GW_CHILD);
  while Child <> 0 do
  begin
    if GetClassName(Child, C, SizeOf(C)) > 0 then
    begin
      S := StrPas(C);
      if UpperCase(S) = 'BUTTON' then
        if Visible then
          ShowWindow(Child, SW_SHOWNORMAL)
        else
          ShowWindow(Child, SW_HIDE);
    end;
    Child := GetWindow(Child, GW_HWNDNEXT);
  end;
end;

{***************************************************}

procedure TJvTraybar.ShowStartButton;
begin
  HideStartBtn(True);
end;

{***************************************************}

procedure TJvTraybar.HideStartButton;
begin
  HideStartBtn(False);
end;

end.
