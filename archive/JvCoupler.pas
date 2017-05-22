{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113 dott 1101 att compuserve dott com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s):
Rob den Braasem <>

Last Modified: 2002-07-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
* rewrite to use collection so more labels / controls can be coupled?
* add option to select side? DONE
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCoupler;

interface

uses
  Messages, SysUtils, Classes, Controls, StdCtrls,
  {$IFNDEF COMPILER5_UP}
  JvTypes,
  {$ENDIF}
  JvComponent;

type
  TJvCoupler = class(TJvComponent)
  private
    FSpacing: Integer;
    FLabel: TCustomLabel;
    FControl: TWinControl;
    FOldWndProc: TWndMethod;
    {rdb}
    FPosition: TAnchorKind;

    procedure SetControl(const Value: TWinControl);
    procedure SetLabel(const Value: TCustomLabel);
    {rdb}
    procedure SetPosition(const Value: TAnchorKind);
    procedure SetSpacing(const Value: Integer);
  protected
    procedure MessageHook(var Msg: TMessage);
    procedure AlignLabel;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DisplayLabel: TCustomLabel read FLabel write SetLabel;
    property FocusControl: TWinControl read FControl write SetControl;
    property Spacing: Integer read FSpacing write SetSpacing default 2;
    {rdb}
    property Position: TAnchorKind read FPosition write SetPosition;
  end;

implementation

constructor TJvCoupler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSpacing := 2;
end;

destructor TJvCoupler.Destroy;
begin
  FocusControl := nil;
  inherited Destroy;
end;

procedure TJvCoupler.AlignLabel;
begin
  if Assigned(FLabel) and Assigned(FControl) then
  begin
    case FPosition of
      AkTop:
        with FLabel do
        begin
          Top := FControl.Top - Height - FSpacing;
          Left := FControl.Left;
        end;
      AkLeft:
        with FLabel do
        begin
          Top := FControl.Top + trunc(FControl.Height / 2 - Height / 2);
          Left := FControl.Left - Width - FSpacing;
        end;
      AkRight:
        with FLabel do
        begin
          Top := FControl.Top + trunc(FControl.Height / 2 - Height / 2);
          Left := FControl.Left + FControl.Width + FSpacing + 2;
        end;
      AkBottom:
        with FLabel do
        begin
          Top := FControl.Top + FControl.Height + FSpacing;
          Left := FControl.Left;
        end;
    else
      with FLabel do
      begin
        Top := FControl.Top - Height - FSpacing;
        Left := FControl.Left;
      end;
    end;
  end;

end;

procedure TJvCoupler.MessageHook(var Msg: TMessage);
begin
  FOldWndProc(Msg);
  if Msg.Msg = WM_MOVE then
    AlignLabel
  else
  if Msg.Msg = WM_DESTROY then
    FocusControl := nil;
end;

procedure TJvCoupler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
    if FLabel = AComponent then
      DisplayLabel := nil
    else
    if FControl = AComponent then
      FocusControl := nil;
  inherited;
end;

procedure TJvCoupler.SetControl(const Value: TWinControl);
begin
  if FControl <> Value then
  begin
    if Assigned(FControl) then
    begin
      FControl.WindowProc := FOldWndProc;
      if Assigned(FLabel) then
        TLabel(FLabel).FocusControl := nil;
    end;

    FControl := Value;
    if Assigned(FControl) then
    begin
      FOldWndProc := FControl.WindowProc;
      FControl.WindowProc := MessageHook;
      if Assigned(FLabel) then
      begin
        TLabel(FLabel).FocusControl := FControl;
        AlignLabel;
      end;
    end;
  end;
  AlignLabel;
end;

procedure TJvCoupler.SetLabel(const Value: TCustomLabel);
begin
  if FLabel <> Value then
  begin
    if Assigned(FLabel) then
      TLabel(FLabel).FocusControl := nil;
    FLabel := Value;
    if Assigned(FLabel) then
    begin
      TLabel(FLabel).FocusControl := FControl;
      if Assigned(FControl) then
        AlignLabel;
    end;
  end;
  AlignLabel;
end;

procedure TJvCoupler.SetPosition(const Value: TAnchorKind);
begin
  FPosition := Value;
  AlignLabel;
end;

procedure TJvCoupler.SetSpacing(const Value: Integer);
begin
  FSpacing := Value;
  AlignLabel;
end;

end.

