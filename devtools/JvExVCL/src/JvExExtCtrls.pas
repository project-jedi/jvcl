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

unit JvExExtCtrls;

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  Windows, Messages, Graphics, Controls, Forms, ExtCtrls,
  Classes, SysUtils,
  JvTypes, JvThemes, JVCLVer, JvExControls;

 {$DEFINE NeedMouseEnterLeave}

type
  JV_CONTROL_EVENTS(Shape)
  JV_CONTROL_EVENTS(PaintBox)
  JV_CONTROL_EVENTS(Image)
  JV_CONTROL_EVENTS(Bevel)
  JV_WINCONTROL_EVENTS(CustomPanel)
  JV_WINCONTROL_EVENTS(CustomRadioGroup)

  JV_CONTROL_EVENTS_BEGIN(Splitter)
  JV_CONSTRUCTOR
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  JV_CONTROL_EVENTS_END(Splitter)

  JV_WINCONTROL_EVENTS(CustomControlBar)
  JV_WINCONTROL_EVENTS(ControlBar)
  JV_WINCONTROL_EVENTS(Panel)
  JV_WINCONTROL_EVENTS(RadioGroup)
  JV_WINCONTROL_EVENTS(Page)
  JV_WINCONTROL_EVENTS(Notebook)
  JV_WINCONTROL_EVENTS(Header)
  {$IFDEF COMPILER6_UP}
  JV_CONTROL_EVENTS(BoundLabel)
  JV_WINCONTROL_EVENTS(CustomLabeledEdit)
  JV_WINCONTROL_EVENTS(LabeledEdit)
  JV_WINCONTROL_EVENTS(CustomColorBox)
  JV_WINCONTROL_EVENTS(ColorBox)
  {$ENDIF COMPILER6_UP}


// SplitterMouseDownFix fixes a bug in the VCL that causes the splitter to no
// more work with the control in the left/top of it when the control has a size
// of 0. This is actually a TWinControl.AlignControl bug.
procedure SplitterMouseDownFix(Splitter: TSplitter);

implementation

JV_CONTROL_EVENTS_IMPL(Shape)
JV_CONTROL_EVENTS_IMPL(PaintBox)
JV_CONTROL_EVENTS_IMPL(Image)
JV_CONTROL_EVENTS_IMPL(Bevel)
JV_WINCONTROL_EVENTS_IMPL(CustomPanel)
JV_WINCONTROL_EVENTS_IMPL(CustomRadioGroup)
JV_WINCONTROL_EVENTS_IMPL(CustomControlBar)
JV_WINCONTROL_EVENTS_IMPL(ControlBar)
JV_WINCONTROL_EVENTS_IMPL(Panel)
JV_WINCONTROL_EVENTS_IMPL(RadioGroup)
JV_WINCONTROL_EVENTS_IMPL(Page)
JV_WINCONTROL_EVENTS_IMPL(Notebook)
JV_WINCONTROL_EVENTS_IMPL(Header)
{$IFDEF COMPILER6_UP}
JV_CONTROL_EVENTS_IMPL(BoundLabel)
JV_WINCONTROL_EVENTS_IMPL(CustomLabeledEdit)
JV_WINCONTROL_EVENTS_IMPL(LabeledEdit)
JV_WINCONTROL_EVENTS_IMPL(CustomColorBox)
JV_WINCONTROL_EVENTS_IMPL(ColorBox)
{$ENDIF COMPILER6_UP}

JV_CONTROL_EVENTS_IMPL(Splitter)

procedure TJvExSplitter.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SplitterMouseDownFix(Self);
  inherited MouseDown(Button, Shift, X, Y);
end;


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

end.
