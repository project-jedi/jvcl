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

Contributor(s): André Snepvangers [asn dott att xs4all.nl] (Redesign)

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

type
  JV_CONTROL(Shape)
  JV_CONTROL(PaintBox)
  JV_CONTROL(Image)
  JV_CONTROL(Bevel)
  JV_WINCONTROL(CustomPanel)
  JV_WINCONTROL(CustomRadioGroup)

  JV_CONTROL_BEGIN(Splitter)
  JV_CONSTRUCTOR
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  JV_CONTROL_END(Splitter)

  JV_WINCONTROL(CustomControlBar)
  JV_WINCONTROL(ControlBar)
  JV_WINCONTROL(Panel)
  JV_WINCONTROL(RadioGroup)
  JV_WINCONTROL(Page)
  JV_WINCONTROL(Notebook)
  JV_WINCONTROL(Header)
  {$IFDEF COMPILER6_UP}
  JV_CONTROL(BoundLabel)
  JV_WINCONTROL(CustomLabeledEdit)
  JV_WINCONTROL(LabeledEdit)
  JV_WINCONTROL(CustomColorBox)
  JV_WINCONTROL(ColorBox)
  {$ENDIF COMPILER6_UP}


// SplitterMouseDownFix fixes a bug in the VCL that causes the splitter to no
// more work with the control in the left/top of it when the control has a size
// of 0. This is actually a TWinControl.AlignControl bug.
procedure SplitterMouseDownFix(Splitter: TSplitter);

implementation

JV_CONTROL_IMPL(Shape)
JV_CONTROL_IMPL(PaintBox)
JV_CONTROL_IMPL(Image)
JV_CONTROL_IMPL(Bevel)
JV_WINCONTROL_IMPL(CustomPanel)
JV_WINCONTROL_IMPL(CustomRadioGroup)
JV_WINCONTROL_IMPL(CustomControlBar)
JV_WINCONTROL_IMPL(ControlBar)
JV_WINCONTROL_IMPL(Panel)
JV_WINCONTROL_IMPL(RadioGroup)
JV_WINCONTROL_IMPL(Page)
JV_WINCONTROL_IMPL(Notebook)
JV_WINCONTROL_IMPL(Header)
{$IFDEF COMPILER6_UP}
JV_CONTROL_IMPL(BoundLabel)
JV_WINCONTROL_IMPL(CustomLabeledEdit)
JV_WINCONTROL_IMPL(LabeledEdit)
JV_WINCONTROL_IMPL(CustomColorBox)
JV_WINCONTROL_IMPL(ColorBox)
{$ENDIF COMPILER6_UP}

JV_CONTROL_IMPL(Splitter)

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
