{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSyncSplitter.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113 dott 1101 att compuserve dott com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvSyncSplitter;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Messages, Controls, ExtCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, QControls, QExtCtrls, Types, QWindows,
  {$ENDIF VisualCLX}
  JvSplitter;

type
  TJvSyncSplitter = class(TJvSplitter)
  private
    FPartner: TJvSyncSplitter;
    FForcedSize: Boolean;
    procedure SetPartner(const Value: TJvSyncSplitter);
  protected
    function GetResizeStyle: TResizeStyle;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetResizeStyle(Value: TResizeStyle);
    procedure VerifyPartner;
    {$IFDEF VCL}
    procedure WndProc(var Msg: TMessage); override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    {$ENDIF VisualCLX}
  published
    property Partner: TJvSyncSplitter read FPartner write SetPartner;
    property ResizeStyle: TResizeStyle read GetResizeStyle write SetResizeStyle;
  end;

implementation

uses
  JvTypes, JvResources;

function TJvSyncSplitter.GetResizeStyle: TResizeStyle;
begin
  Result := inherited ResizeStyle
end;

procedure TJvSyncSplitter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = Partner) then
    Partner := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TJvSyncSplitter.SetPartner(const Value: TJvSyncSplitter);
begin
  if Value <> Self then
  begin
    FPartner := Value;
    VerifyPartner;
  end
  else
    raise EJVCLException.CreateRes(@RsEInvalidPartner);
end;

procedure TJvSyncSplitter.SetResizeStyle(Value: TResizeStyle);
begin
  inherited ResizeStyle := Value;
  VerifyPartner;
end;

procedure TJvSyncSplitter.VerifyPartner;
begin
  if csDesigning in ComponentState then
    if Assigned(Partner) then
      if ((Partner.ResizeStyle = rsUpdate) and (ResizeStyle <> rsUpdate)) or
         ((Partner.ResizeStyle <> rsUpdate) and (ResizeStyle = rsUpdate)) then
        {if MessageDlg(Format('Current ResizeStyle settings for %s and %s will'
          + ' cause problems at runtime. Change both to rsUpdate?',
          [Name, Partner.Name]), mtWarning, [mbYes,mbNo], 0) = mrYes then}
        begin
          if Partner.ResizeStyle = rsUpdate then
            ResizeStyle := rsUpdate
          else
            Partner.ResizeStyle := rsUpdate;
        end;
end;

{$IFDEF VCL}
procedure TJvSyncSplitter.WndProc(var Msg: TMessage);
begin
  if Assigned(FPartner) and not FForcedSize and not (csDesigning in ComponentState) then
    case Msg.Msg of
      WM_MOUSEFIRST..WM_MOUSELAST:
        begin
          Partner.FForcedSize := True;
          try
            Partner.Perform(Msg.Msg, Msg.WParam, Msg.LParam);
          finally
            Partner.FForcedSize := False;
          end;
        end;
    end;
  inherited WndProc(Msg);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
procedure TJvSyncSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Assigned(FPartner) and not FForcedSize and not (csDesigning in ComponentState) then
  begin
    Partner.FForcedSize := True;
    try
      Partner.MouseDown(Button, Shift, X, Y);
    finally
      Partner.FForcedSize := False;
    end;
  end;
end;

procedure TJvSyncSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FPartner) and not FForcedSize and not (csDesigning in ComponentState) then
  begin
    Partner.FForcedSize := True;
    try
      Partner.MouseMove(Shift, X, Y);
    finally
      Partner.FForcedSize := False;
    end;
  end;
end;

procedure TJvSyncSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Assigned(FPartner) and not FForcedSize and not (csDesigning in ComponentState) then
  begin
    Partner.FForcedSize := True;
    try
      Partner.MouseUp(Button, Shift, X, Y);
    finally
      Partner.FForcedSize := False;
    end;
  end;
end;

procedure TJvSyncSplitter.MouseEnter(AControl: TControl);
begin
  if Assigned(FPartner) and not FForcedSize and not (csDesigning in ComponentState) then
  begin
    Partner.FForcedSize := True;
    try
      Partner.MouseEnter(AControl);
    finally
      Partner.FForcedSize := False;
    end;
  end;
end;

procedure TJvSyncSplitter.MouseLeave(AControl: TControl);
begin
  if Assigned(FPartner) and not FForcedSize and not (csDesigning in ComponentState) then
  begin
    Partner.FForcedSize := True;
    try
      Partner.MouseLeave(AControl);
    finally
      Partner.FForcedSize := False;
    end;
  end;
end;
{$ENDIF VCL}

end.

