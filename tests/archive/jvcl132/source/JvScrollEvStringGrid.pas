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


unit JvScrollEvStringGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,  Grids ,JVCLVer;


type
  TBeforeScrollEvent = Procedure( Sender: TWinControl; scrollcode: Integer;
                                  Var allowScroll: Boolean ) of Object;
  TAfterScrollEvent  = Procedure( Sender: TwinControl; scrollcode: Integer )
                         of Object;

  TJvScrollEvStringGrid = class(TStringgrid)
  private
    FAfterVScroll: TAfterScrollEvent;
    FAfterHScroll: TAfterScrollEvent;
    FBeforeVScroll: TBeforeScrollEvent;
    FBeforeHScroll: TBeforeScrollEvent;
    FAboutJVCL: TJVCLAboutInfo;

    Procedure WMVScroll( Var msg: TWMVScroll ); message WM_VSCROLL;
    Procedure WMHScroll( Var msg: TWMHScroll ); message WM_HSCROLL;
  protected
    Function DoBeforeScroll( msg: Cardinal; scrollcode: Integer): Boolean; virtual;
    Procedure DoAfterScroll( msg: Cardinal; scrollcode: Integer ); virtual;
  public
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL  stored False;
    Property OnBeforeHScroll: TBeforeScrollEvent
      read FBeforeHScroll write FBeforeHScroll;
    Property OnBeforeVScroll: TBeforeScrollEvent
      read FBeforeVScroll write FBeforeVScroll;
    Property OnAfterHScroll: TAfterScrollEvent
      read FAfterHScroll write FAfterHScroll;
    Property OnAfterVScroll: TAfterScrollEvent
      read FAfterVScroll write FAfterVScroll;
  end;


implementation


{ TJvScrollEvStringGrid }

procedure TJvScrollEvStringGrid.DoAfterScroll(msg: Cardinal;
  scrollcode: Integer);
Var
  proc: TAfterScrollEvent;
begin
  If msg = WM_VSCROLL Then
    proc := FAfterVScroll
  Else
    proc := FAfterHScroll;
  If Assigned( proc ) Then
    proc( self, scrollcode );
end;

function TJvScrollEvStringGrid.DoBeforeScroll(msg: Cardinal;
  scrollcode: Integer): Boolean;
Var
  proc: TBeforeScrollEvent;
begin
  If msg = WM_VSCROLL Then
    proc := FBeforeVScroll
  Else
    proc := FBeforeHScroll;
  Result := True;
  If Assigned( proc ) Then
    proc( self, scrollcode, result );
end;

procedure TJvScrollEvStringGrid.WMHScroll(var msg: TWMHScroll);
begin
  If DoBeforeScroll( msg.Msg, msg.ScrollCode ) Then Begin
    inherited;
    DoAfterScroll( msg.Msg, msg.ScrollCode );
  End
  Else
    msg.result := 0;
end;

procedure TJvScrollEvStringGrid.WMVScroll(var msg: TWMVScroll);
begin
  If DoBeforeScroll( msg.Msg, msg.ScrollCode ) Then Begin
    inherited;
    DoAfterScroll( msg.Msg, msg.ScrollCode );
  End
  Else
    msg.result := 0;
end;

end.
