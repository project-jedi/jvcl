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

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JvKeyScrollBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs ,JVCLVer;



type
  TJvKeyScrollBox = class(TScrollBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    { Private declarations }
    Procedure WMGetDlgCode( Var msg: TWMGetDlgCode );
      message WM_GETDLGCODE;
  protected
    { Protected declarations }
    Procedure WndProc( Var Msg: TMessage ); override; 
  public
    { Public declarations }
    Procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL  stored False;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyup;
    property tabStop;
  end;

implementation

{ TJvKeyScrollBox }

procedure TJvKeyScrollBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  If Key <> 0 Then
    Case Key of
      VK_UP  : Perform( WM_VSCROLL, SB_LINEUP, 0 );
      VK_DOWN: Perform( WM_VSCROLL, SB_LINEDOWN, 0 );
      VK_LEFT: Perform( WM_HSCROLL, SB_LINELEFT, 0 );
      VK_RIGHT:Perform( WM_HSCROLL, SB_LINERIGHT, 0 );
      VK_NEXT :If ssShift In Shift Then
                 Perform( WM_HSCROLL, SB_PAGERIGHT, 0 )
               Else
                 Perform( WM_VSCROLL, SB_PAGEDOWN, 0 );
      VK_PRIOR:If ssShift In Shift Then
                 Perform( WM_HSCROLL, SB_PAGELEFT, 0 )
               Else
                 Perform( WM_VSCROLL, SB_PAGEUP, 0 );
      VK_HOME :If ssCtrl In Shift Then
                 Perform( WM_VSCROLL, SB_TOP, 0 )
               Else
                 Perform( WM_HSCROLL, SB_LEFT, 0 );
      VK_END  :If ssCtrl In Shift Then
                 Perform( WM_VSCROLL, SB_BOTTOM, 0 )
               Else
                 Perform( WM_HSCROLL, SB_RIGHT, 0 );
   End; 
end;

procedure TJvKeyScrollBox.WMGetDlgCode(var msg: TWMGetDlgCode);
begin
  msg.result := DLGC_WANTALLKEYS or DLGC_WANTARROWS;
end;

procedure TJvKeyScrollBox.WndProc(var Msg: TMessage);
begin
  If Msg.Msg = WM_LBUTTONDOWN Then
    If not Focused Then SetFocus;
  inherited;
end;

end.
