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

unit JvScrollEvStringGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Grids, JVCLVer;

type
  TBeforeScrollEvent = procedure(Sender: TWinControl; scrollcode: Integer;
    var AllowScroll: Boolean) of object;
  TAfterScrollEvent = procedure(Sender: TwinControl; scrollcode: Integer)
    of object;

  TJvScrollEvStringGrid = class(TStringgrid)
  private
    FAfterVScroll: TAfterScrollEvent;
    FAfterHScroll: TAfterScrollEvent;
    FBeforeVScroll: TBeforeScrollEvent;
    FBeforeHScroll: TBeforeScrollEvent;
    FAboutJVCL: TJVCLAboutInfo;

    procedure WMVScroll(var msg: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var msg: TWMHScroll); message WM_HSCROLL;
  protected
    function DoBeforeScroll(msg: Cardinal; scrollcode: Integer): Boolean; virtual;
    procedure DoAfterScroll(msg: Cardinal; scrollcode: Integer); virtual;
  public
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property OnBeforeHScroll: TBeforeScrollEvent
      read FBeforeHScroll write FBeforeHScroll;
    property OnBeforeVScroll: TBeforeScrollEvent
      read FBeforeVScroll write FBeforeVScroll;
    property OnAfterHScroll: TAfterScrollEvent
      read FAfterHScroll write FAfterHScroll;
    property OnAfterVScroll: TAfterScrollEvent
      read FAfterVScroll write FAfterVScroll;
  end;

implementation

{ TJvScrollEvStringGrid }

procedure TJvScrollEvStringGrid.DoAfterScroll(msg: Cardinal;
  scrollcode: Integer);
var
  proc: TAfterScrollEvent;
begin
  if msg = WM_VSCROLL then
    proc := FAfterVScroll
  else
    proc := FAfterHScroll;
  if Assigned(proc) then
    proc(self, scrollcode);
end;

function TJvScrollEvStringGrid.DoBeforeScroll(msg: Cardinal;
  scrollcode: Integer): Boolean;
var
  proc: TBeforeScrollEvent;
begin
  if msg = WM_VSCROLL then
    proc := FBeforeVScroll
  else
    proc := FBeforeHScroll;
  Result := True;
  if Assigned(proc) then
    proc(self, scrollcode, result);
end;

procedure TJvScrollEvStringGrid.WMHScroll(var msg: TWMHScroll);
begin
  if DoBeforeScroll(msg.Msg, msg.ScrollCode) then
  begin
    inherited;
    DoAfterScroll(msg.Msg, msg.ScrollCode);
  end
  else
    msg.result := 0;
end;

procedure TJvScrollEvStringGrid.WMVScroll(var msg: TWMVScroll);
begin
  if DoBeforeScroll(msg.Msg, msg.ScrollCode) then
  begin
    inherited;
    DoAfterScroll(msg.Msg, msg.ScrollCode);
  end
  else
    msg.result := 0;
end;

end.
