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

{$I JVCL.INC}

unit JvMousePanel;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls,
  JVCLVer;

type
  TJvMousePanel = class(TPanel)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOver: Boolean;
    FOldColor: TColor;
    FMouseOverColor: TColor;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure SetMouseOverColor(const Value: TColor);
  protected
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;
    // (rom) renamed needs further work
    property MouseOver: Boolean read FOver;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property MouseOverColor: TColor read FMouseOverColor write SetMouseOverColor;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

implementation

uses
  JvMouseTimerU;

constructor TJvMousePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // set default for mouse over color
  FMouseOverColor := Color;
end;

procedure TJvMousePanel.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not MouseOver then
  begin
    FOldColor := Color;
    FOver := True;
    Color := MouseOverColor; // invalidates control
    MouseTimer.Attach(Self);
  end;
  DoMouseEnter;
end;

procedure TJvMousePanel.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if MouseOver then
  begin
    FOver := False;
    Color := FOldColor; // invalidates control
    MouseTimer.Detach(Self);
  end;
  DoMouseLeave;
end;

procedure TJvMousePanel.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvMousePanel.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvMousePanel.SetMouseOverColor(const Value: TColor);
begin
  if FMouseOverColor <> Value then
  begin
    FMouseOverColor := Value;
    if MouseOver then
      Color := value;
  end;
end;

end.

