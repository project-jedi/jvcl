{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormPlace.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFormPlace;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, Registry,
  JvComponent;

type
  TJvFormPlace = class(TJvComponent)
  private
    FRemember: Boolean;
    FForm: TCustomForm;
    FTImer: TTimer;
    procedure Rememb(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Remember: Boolean read FRemember write FRemember default True;
  end;

implementation

uses
  MultiMon, Math;

resourcestring
  // (rom) changed to JVCL
  RC_FormPlace = 'Software\JVCL\FormPlace\';

constructor TJvFormPlace.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRemember := True;
  FTimer := TTimer.Create(Self);
  FForm := GetParentForm(TControl(AOwner));
  if not (csDesigning in ComponentState) and FRemember then
  begin
    // (rom) i do not like this. a tight timer is not a good idea
    FTimer.OnTimer := Rememb;
    FTimer.Interval := 10;
    FTimer.Enabled := True;
  end;
end;

destructor TJvFormPlace.Destroy;
var
  Nam: string;
  NLeft, NTop, NWidth, NHeight: Integer;
begin
  if FRemember then
    if not (csDesigning in ComponentState) then
      with TRegistry.Create do
      begin
        OpenKey(RC_FormPlace, True);
        Nam := Application.Title + '_' + FForm.Name;
        NWidth := FForm.Width;
        NHeight := FForm.Height;
        NLeft := FForm.Left;
        NTop := FForm.Top;
        WriteInteger(Nam + '_left', NLeft);
        WriteInteger(Nam + '_top', NTop);
        WriteInteger(Nam + '_width', NWidth);
        WriteInteger(Nam + '_height', NHeight);
        Free;
      end;
  FTimer.Free;
  inherited Destroy;
end;

function MinMonitorLeft: integer;
var
  I: Integer;
begin
  Result := 0;
  with Screen do
    for I := 0 to MonitorCount - 1 do
      Result := Min(Result, Monitors[I].Left);
end;

function MaxMonitorRight: integer;
var
  I: Integer;
begin
  with Screen do
  begin
    Result := Width;
    for I := 0 to MonitorCount - 1 do
      Result := Max(Result, Monitors[I].Left + Monitors[I].Width);
  end;
end;

function MinMonitorTop: integer;
var
  I: Integer;
begin
  with Screen do
  begin
    Result := 0;
    for I := 0 to MonitorCount - 1 do
      Result := Min(Result, Monitors[I].Top);
  end;
end;

function MaxMonitorBottom: integer;
var
  I: Integer;
begin
  with Screen do
  begin
    Result := Height;
    for I := 0 to MonitorCount - 1 do
      Result := Max(Result, Monitors[I].Top + Monitors[I].Height);
  end;
end;

function MonitorFromPoint(APoint: TPoint): TMonitor;
{$IFNDEF COMPILER6_UP}
var
  H: HMONITOR;
  I: Integer;
{$ENDIF}
begin
  {$IFDEF COMPILER6_UP}
  Result := Screen.MonitorFromPoint(APoint);
  {$ELSE}
  H := MultiMon.MonitorFromPoint(APoint, MONITOR_DEFAULTTONEAREST);
  Result := nil;
  for I := 0 to Screen.MonitorCount - 1 do
    if Screen.Monitors[I].Handle = H then
    begin
      Result := Screen.Monitors[I];
      Exit;
    end;
  {$ENDIF}
end;

procedure TJvFormPlace.Rememb(Sender: TObject);
var
  Nam: string;
  NLeft, NTop, NWidth, NHeight: Integer;
  M: TMonitor;
begin
  FTimer.Enabled := False;
  if FRemember then
  begin
    with TRegistry.Create do
    begin
      OpenKey(RC_FormPlace, True);
      Nam := Application.Title + '_' + FForm.Name;

      if TForm(FForm).FormStyle = fsMdiChild then
      begin
        if ValueExists(Nam + '_left') then
          NLeft := ReadInteger(Nam + '_left')
        else
          NLeft := Application.MainForm.Width + 1;
        if ValueExists(Nam + '_top') then
          NTop := ReadInteger(Nam + '_top')
        else
          NTop := Application.MainForm.Height + 1;
        if ValueExists(Nam + '_width') then
          NWidth := ReadInteger(Nam + '_width')
        else
          NWidth := Application.MainForm.Width + 1;
        if ValueExists(Nam + '_height') then
          NHeight := ReadInteger(Nam + '_height')
        else
          NHeight := Application.MainForm.Height + 1;
        if (NLeft > Application.MainForm.Width) or (NTop > Application.MainForm.Height) or
          (NLeft < 0) or (NTop < 0) then
        begin
          NWidth := FForm.Width;
          NHeight := FForm.Height;
          NLeft := (Application.MainForm.Width - NWidth) div 2;
          NTop := (Application.MainForm.Height - NHeight) div 2;
        end;
      end
      else
      begin
        if ValueExists(Nam + '_left') then
          NLeft := ReadInteger(Nam + '_left')
        else
          NLeft := Screen.Width + 1;
        if ValueExists(Nam + '_top') then
          NTop := ReadInteger(Nam + '_top')
        else
          NTop := Screen.Height + 1;
        if ValueExists(Nam + '_width') then
          NWidth := ReadInteger(Nam + '_width')
        else
          NWidth := Screen.Width + 1;
        if ValueExists(Nam + '_height') then
          NHeight := ReadInteger(Nam + '_height')
        else
          NHeight := Screen.Height + 1;
        if (NLeft > MaxMonitorRight) or (NTop > MaxMonitorBottom) or
          (NLeft < MinMonitorLeft) or (NTop < MinMonitorTop) then
        begin
          NWidth := FForm.Width;
          NHeight := FForm.Height;
          M := MonitorFromPoint(Point(NLeft, NTop));
          if M <> nil then
          begin
            NLeft := M.Left + (M.Width - NWidth) div 2;
            NTop := M.Top + (M.Height - NHeight) div 2;
          end
          else
          begin
            NLeft := (Screen.Width - NWidth) div 2;
            NTop := (Screen.Height - NHeight) div 2;
          end;
        end;
      end;
      FForm.Left := NLeft;
      FForm.Width := NWidth;
      FForm.Height := NHeight;
      FForm.Top := NTop;
      Free;
    end;
  end;
end;

end.

