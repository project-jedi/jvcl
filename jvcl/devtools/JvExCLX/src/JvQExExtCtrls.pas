{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExExtCtrls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQExExtCtrls;

{$I jvcl.inc}
{MACROINCLUDE JvQExControls.macros}

WARNINGHEADER

interface

uses
  QGraphics, QControls, QForms, QExtCtrls, QNotebook,
  Qt, QWindows, QMessages,
  Classes, SysUtils,
  JvQTypes, JvQThemes, JVCLXVer, JvQExControls;

type
  JV_CONTROL(Shape)
  JV_CONTROL(PaintBox)
  JV_CONTROL(Image)
  JV_CONTROL(Bevel)
  JV_CUSTOMCONTROL_BEGIN(CustomControlBar)
    function HitTest(X, Y: Integer): Boolean; reintroduce;
  JV_CUSTOMCONTROL_END(CustomControlBar)
  JV_CUSTOMCONTROL_BEGIN(ControlBar)
    function HitTest(X, Y: Integer): Boolean; reintroduce;
  JV_CUSTOMCONTROL_END(ControlBar)
  JV_CUSTOMCONTROL(CustomPanel)
  JV_CUSTOMCONTROL(Panel)
  JV_WINCONTROL(CustomRadioGroup)
  JV_WINCONTROL(RadioGroup)
  JV_CUSTOMCONTROL(Page)
  JV_CUSTOMCONTROL(Notebook)

  JV_CONTROL_BEGIN(Splitter)
//  JV_CONSTRUCTOR
  private
    FText: string;
  protected
    function GetText: TCaption; override;
    procedure SetText(const Value: TCaption); override;
//  protected
//    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  JV_CONTROL_END(Splitter)

//  JV_CUSTOMCONTROL_BEGIN(TCustomControlBar)
//  JV_CONSTRUCTOR
//  protected
//    function HitTest(X, Y: Integer): Boolean; override;
//  JV_CUSTOMCONTROL_END(CustomControlBar)

//  JV_CUSTOMCONTROL(Header)
(*
// SplitterMouseDownFix fixes a bug in the VCL that causes the splitter to no
// more work with the control in the left/top of it when the control has a size
// of 0. This is actually a TWinControl.AlignControl bug.
procedure SplitterMouseDownFix(Splitter: TSplitter);
*)
implementation

JV_CONTROL_IMPL(Shape)
JV_CONTROL_IMPL(PaintBox)
JV_CONTROL_IMPL(Image)
JV_CONTROL_IMPL(Bevel)

function TJvExCustomControlBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height);
end;

JV_CUSTOMCONTROL_IMPL(CustomControlBar)

function TJvExControlBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height);
end;

JV_CUSTOMCONTROL_IMPL(ControlBar)

JV_CUSTOMCONTROL_IMPL(Notebook)
JV_CUSTOMCONTROL_IMPL(CustomPanel)
JV_CUSTOMCONTROL_IMPL(Panel)
JV_CUSTOMCONTROL_IMPL(Page)
JV_WINCONTROL_IMPL(CustomRadioGroup)
JV_WINCONTROL_IMPL(RadioGroup)


{$UNDEF CONSTRUCTORCODE}
{$DEFINE CONSTRUCTORCODE ControlStyle := ControlStyle + [csSetCaption];}

function TJvExSplitter.GetText: TCaption;
begin
  Result := FText;
end;

procedure TJvExSplitter.SetText(const Value: TCaption);
begin
  if Value <> FText then
  begin
    FText := Value;
    TextChanged;
  end;
end;

JV_CONTROL_IMPL(Splitter)
{$UNDEF CONSTRUCTORCODE}
{$DEFINE CONSTRUCTORCODE}
(*
function TJvExCustomControlBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height);
end;
*)



// JV_CUSTOMCONTROL_IMPL(Header)

(*
// SplitterMouseDownFix fixes a bug in the VCL that causes the splitter to no
// more work with the control in the left/top of it when the control has a size
// of 0. This is actually a TWinControl.AlignControl bug.
procedure SplitterMouseDownFix(Splitter: TSplitter);
var
  Control: TControl;
  Pt: TPoint;
  R: TRect;
  I, Size: Integer;
begin
  with Splitter do
  begin
    if Align in [alLeft, alTop] then
    begin
      Control := nil;
      Pt := Point(Left, Top);
      if Align = alLeft then
        Dec(Pt.X)
      else //if Align = alTop then
        Dec(Pt.Y);

      for I := 0 to Parent.ControlCount - 1 do
      begin
        Control := Parent.Controls[I];
        R := Control.BoundsRect;
        if Align = alLeft then
          Size := R.Right - R.Left
        else //if Align = alTop then
          Size := R.Bottom - R.Top;

        if Control.Visible and Control.Enabled and (Size = 0) then
        begin
          if Align = alLeft then
            Dec(R.Left)
          else // Align = alTop then
            Dec(R.Top);

          if PtInRect(R, Pt) then
            Break;
        end;
        Control := nil;
      end;

      if Control = nil then
      begin
        // Check for the control that is zero-sized but after the splitter.
        // TWinControl.AlignControls does not work properly with alLeft/alTop.
        if Align = alLeft then
          Pt := Point(Left + Width - 1, Top)
        else // if Align = alTop then
          Pt := Point(Left, Top + Height - 1);

        for I := 0 to Parent.ControlCount - 1 do
        begin
          Control := Parent.Controls[I];
          R := Control.BoundsRect;
          if Align = alLeft then
            Size := R.Right - R.Left
          else //if Align = alTop then
            Size := R.Bottom - R.Top;

          if Control.Visible and Control.Enabled and (Size = 0) then
          begin
            if Align = alLeft then
              Dec(R.Left)
            else // Align = alTop then
              Dec(R.Top);

            if PtInRect(R, Pt) then
              Break;
          end;
          Control := nil;
        end;

        if Control <> nil then
        begin
          // realign left/top control
          if Align = alLeft then
            Control.Left := -1
          else // if Align = alTop then
            Control.Top := -1;
        end;
      end;
    end;
  end;
end;

procedure TJvExSplitter.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SplitterMouseDownFix(Self);
  inherited MouseDown(Button, Shift, X, Y);
end;
*)

UNITVERSION

end.
