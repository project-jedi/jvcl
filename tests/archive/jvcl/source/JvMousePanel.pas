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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, JVCLVer;

type
  TJvMousePanel = class(TPanel)
  private
    { Private declarations }
    FMouseEnter, FMouseLeave: TNotifyEvent;
    FMouseInControl: Boolean;
    FOldColor: TColor;
    FMouseOverColor: TColor;
    FAboutJVCL: TJVCLAboutInfo;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetMouseOverColor(const Value: TColor);
  protected
    { Protected declarations }
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;

    property MouseInControl: Boolean read FMouseInControl;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property MouseOverColor: TColor read FMouseoverColor write SetMouseOverColor;
    property OnMouseEnter: TNotifyEvent read FMouseEnter write FMouseEnter;
    property OnMouseLeave: TNotifyEvent read FMouseLeave write FMouseLeave;
  end;

implementation

uses JvMouseTimerU;

{ TJvMousePanel }

procedure TJvMousePanel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not MouseInControl then
  begin
    FOldColor := Color;
    FMouseInControl := true;
    Color := MouseOverColor; // invalidates control
    Mousetimer.Attach(self);
  end;
  DoMouseEnter;
end;

procedure TJvMousePanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if MouseInControl then
  begin
    FMouseInControl := false;
    Color := FOldColor; // invalidates control
    Mousetimer.Detach(self);
  end;
  DoMouseLeave;
end;

constructor TJvMousePanel.Create(aOwner: TComponent);
begin
  inherited;
  // set default for mouse over color
  FMouseOverColor := Color;
end;

procedure TJvMousePanel.DoMouseEnter;
begin
  if Assigned(FMouseEnter) then
    FMouseEnter(self);
end;

procedure TJvMousePanel.DoMouseLeave;
begin
  if Assigned(FMouseLeave) then
    FMouseLeave(self);
end;

procedure TJvMousePanel.SetMouseOverColor(const Value: TColor);
begin
  if FMouseoverColor <> Value then
  begin
    FMouseoverColor := Value;
    if MouseInControl then
      Color := value;
  end;
end;

end.
