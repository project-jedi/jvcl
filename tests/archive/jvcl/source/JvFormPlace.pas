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
  ExtCtrls, Registry, JvComponent;

type
  TJvFormPlace = class(TJvComponent)
  private
    FBoolean: Boolean;
    FForm: TCustomForm;
    FTImer: TTimer;
    procedure Rememb(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Remember: Boolean read FBoolean write FBoolean default True;
  end;

implementation
uses
  MultiMon, JvMaxMin;

resourcestring
  RC_FormPlace = 'Software\BuyPin\FormPlace\';

  {**************************************************}

constructor TJvFormPlace.Create(AOwner: TComponent);
begin
  inherited;
  FBoolean := True;
  FTimer := TTimer.Create(Self);
  FForm := GetParentForm(TControl(AOwner));
  if not (csDesigning in ComponentState) and FBoolean then
  begin
    FTimer.OnTimer := Rememb;
    FTimer.Interval := 10;
    FTimer.Enabled := True;
  end;
end;

{**************************************************}

destructor TJvFormPlace.Destroy;
var
  nam: string;
  fleft, ftop, fwidth, fheight: Integer;
begin
  if FBoolean then
    if not (csDesigning in ComponentState) then
      with TRegistry.Create do
      begin
        OpenKey(RC_FormPlace, True);
        nam := Application.Title + '_' + FForm.Name;
        fwidth := FForm.Width;
        fheight := FForm.Height;
        fleft := FForm.Left;
        ftop := FForm.Top;
        WriteInteger(nam + '_left', fleft);
        WriteInteger(nam + '_top', ftop);
        WriteInteger(nam + '_width', fwidth);
        WriteInteger(nam + '_height', fheight);
        Free;
      end;
  FTimer.Free;
  inherited;
end;

{**************************************************}

function MinMonitorLeft:integer;
var i:integer;
begin
  Result := 0;
  with Screen do
    for i := 0 to MonitorCount - 1 do
      Result := Min(Result,Monitors[i].Left);
end;

function MaxMonitorRight:integer;
var i:integer;
begin
  with Screen do
  begin
    Result := Width;
    for i := 0 to MonitorCount - 1 do
      Result := Max(Result,Monitors[i].Left + Monitors[i].Width);
  end;
end;

function MinMonitorTop:integer;
var i:integer;
begin
  with Screen do
  begin
    Result := 0;
    for i := 0 to MonitorCount - 1 do
      Result := Min(Result,Monitors[i].Top);
  end;
end;

function MaxMonitorBottom:integer;
var i:integer;
begin
  with Screen do
  begin
    Result := Height;
    for i := 0 to MonitorCount - 1 do
      Result := Max(Result,Monitors[i].Top + Monitors[i].Height);
  end;
end;

function MonitorFromPoint(APoint:TPoint):TMonitor;
{$IFNDEF COMPILER6_UP}var H:HMONITOR;i:integer;{$ENDIF}
begin
  {$IFDEF COMPILER6_UP}
  Result := Screen.MonitorFromPoint(APoint);
  {$ELSE}
  H := MultiMon.MonitorFromPoint(APoint,MONITOR_DEFAULTTOPRIMARY);
  Result := nil;
  for i := 0 to Screen.MonitorCount - 1 do
    if Screen.Monitors[i].Handle = H then
      begin
        Result := Screen.Monitors[i];
        Exit;
      end;
  {$ENDIF}
end;

procedure TJvFormPlace.Rememb(Sender: TObject);
var
  nam: string;
  fleft, ftop, fwidth, fheight: Integer;
  M:TMonitor;
begin
  Ftimer.Enabled := False;
  if FBoolean then
  begin
    with TRegistry.Create do
    begin
      OpenKey(RC_FormPlace, True);
      nam := Application.Title + '_' + FForm.Name;

      if TForm(FForm).FormStyle = fsMdiChild then
      begin
        if ValueExists(nam + '_left') then
          fleft := ReadInteger(nam + '_left')
        else
          fleft := Application.MainForm.Width + 1;
        if ValueExists(nam + '_top') then
          ftop := ReadInteger(nam + '_top')
        else
          ftop := Application.MainForm.Height + 1;
        if ValueExists(nam + '_width') then
          fwidth := ReadInteger(nam + '_width')
        else
          fwidth := Application.MainForm.Width + 1;
        if ValueExists(nam + '_height') then
          fheight := ReadInteger(nam + '_height')
        else
          fheight := Application.MainForm.Height + 1;
        if (fleft > Application.MainForm.Width) or (ftop > Application.MainForm.Height) or
          (fleft < 0) or (ftop < 0) then
        begin
          fwidth := FForm.Width;
          fheight := FForm.Height;
          fleft := (Application.MainForm.Width - fwidth) div 2;
          ftop := (Application.MainForm.Height - fheight) div 2;
        end;
      end
      else
      begin
        if ValueExists(nam + '_left') then
          fleft := ReadInteger(nam + '_left')
        else
          fleft := Screen.Width + 1;
        if ValueExists(nam + '_top') then
          ftop := ReadInteger(nam + '_top')
        else
          ftop := Screen.Height + 1;
        if ValueExists(nam + '_width') then
          fwidth := ReadInteger(nam + '_width')
        else
          fwidth := Screen.Width + 1;
        if ValueExists(nam + '_height') then
          fheight := ReadInteger(nam + '_height')
        else
          fheight := Screen.Height + 1;
        if (fleft > MaxMonitorRight) or (ftop > MaxMonitorBottom) or
          (fleft < MinMonitorLeft) or (ftop < MinMonitorTop) then
        begin
          fwidth := FForm.Width;
          fheight := FForm.Height;
          M := MonitorFromPoint(Point(fleft,ftop));
          if M <> nil then
          begin
            fleft := M.Left + (M.Width - fwidth) div 2;
            ftop := M.Top + (M.Height - fheight) div 2;
          end
          else
          begin
            fleft := (Screen.Width - fwidth) div 2;
            ftop := (Screen.Height - fheight) div 2;
          end;
        end;
      end;
      FForm.Left := fleft;
      FForm.Width := fwidth;
      FForm.Height := fheight;
      FForm.Top := ftop;
      Free;
    end;
  end;
end;

end.
