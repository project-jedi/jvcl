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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Stdctrls, JvComponent
{$IFNDEF COMPILER5_UP}, JvTypes{$ENDIF};

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
    { Private declarations }
  protected
    { Protected declarations }
    procedure MessageHook(var msg: TMessage);
    procedure AlignLabel;

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    { Public declarations }
    constructor Create(aOWner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property DisplayLabel: TCustomLabel read FLabel write SetLabel;
    property FocusControl: TWinControl read FControl write SetControl;
    property Spacing: Integer read FSpacing write SetSpacing default 2;
    {rdb}
    property Position: TAnchorKind read FPosition write SetPosition;
  end;

implementation

{ TJvCoupler }

procedure TJvCoupler.AlignLabel;
begin
  if (Assigned(FLabel) and Assigned(FControl)) then
  begin
    case FPosition of
      AkTop:
        with FLabel do
        begin
          top := FControl.Top - Height - FSpacing;
          Left := Fcontrol.Left;
        end;

      AkLeft:
        with FLabel do
        begin
          top := FControl.Top + trunc(Fcontrol.Height / 2 - Height / 2);
          Left := Fcontrol.Left - Width - Fspacing;
        end;

      AkRight:
        with FLabel do
        begin
          top := FControl.Top + trunc(Fcontrol.Height / 2 - Height / 2);
          Left := Fcontrol.Left + Fcontrol.Width + Fspacing + 2;
        end;

      AkBottom:
        with FLabel do
        begin
          top := FControl.Top + Fcontrol.Height + FSpacing;
          Left := Fcontrol.Left;
        end;

    else
      with FLabel do
      begin
        top := FControl.Top - Height - FSpacing;
        Left := Fcontrol.Left;
      end;
    end;
  end;

end;

constructor TJvCoupler.Create(aOWner: TComponent);
begin
  inherited;
  FSpacing := 2;
end;

destructor TJvCoupler.Destroy;
begin
  FocusControl := nil;
  inherited;
end;

procedure TJvCoupler.MessageHook(var msg: TMessage);
begin
  FOldWndProc(msg);
  if msg.Msg = WM_MOVE then
    AlignLabel
  else if msg.Msg = WM_DESTROY then
    FocusControl := nil;
end;

procedure TJvCoupler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
    if FLabel = AComponent then
      DisplayLabel := nil
    else if FControl = AComponent then
      FocusControl := nil;
  inherited;
end;

procedure TJvCoupler.SetControl(const Value: TWinControl);
begin
  if Fcontrol <> Value then
  begin
    if Assigned(FControl) then
    begin
      FControl.WindowProc := FOldWndProc;
      if Assigned(FLabel) then
        TLabel(FLabel).FocusControl := nil;
    end;                                { If }

    FControl := Value;
    if Assigned(FControl) then
    begin
      FOldWndProc := FControl.WindowProc;
      FControl.WindowProc := MessageHook;
      if Assigned(FLabel) then
      begin
        TLabel(FLabel).FocusControl := FControl;
        AlignLabel;
      end;                              { If }
    end;                                { If }
  end;                                  { If }
  AlignLabel;
end;                                    { TJvCoupler.SetControl }

procedure TJvCoupler.SetLabel(const Value: TCustomLabel);
begin
  if FLabel <> value then
  begin
    if Assigned(FLabel) then
      TLabel(FLabel).FocusControl := nil;
    FLabel := Value;
    if Assigned(FLabel) then
    begin
      TLabel(FLabel).FocusControl := FControl;
      if Assigned(FControl) then
        AlignLabel;
    end;                                { If }
  end;                                  { If }
  AlignLabel;
end;                                    { TJvCoupler.SetLabel }

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

