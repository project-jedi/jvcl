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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExExtCtrls;

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  Windows, Messages, Types,
  SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvTypes, JvThemes, JVCLVer, JvExControls;

type
  CONTROL_DECL_DEFAULT(Shape)

  CONTROL_DECL_DEFAULT(PaintBox)

  CONTROL_DECL_DEFAULT(Image)

  CONTROL_DECL_DEFAULT(Bevel)

  WINCONTROL_DECL_DEFAULT(CustomPanel)

  TJvExPubCustomPanel = class(TJvExCustomPanel)
  COMMON_PUBLISHED
  end;
  
  WINCONTROL_DECL_DEFAULT(CustomRadioGroup)

  TJvExSplitter = class(TSplitter, IJvExControl)
  CONTROL_DECL
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  WINCONTROL_DECL_DEFAULT(CustomControlBar)

  WINCONTROL_DECL_DEFAULT(ControlBar)

  WINCONTROL_DECL_DEFAULT(Panel)

  WINCONTROL_DECL_DEFAULT(RadioGroup)

  WINCONTROL_DECL_DEFAULT(Page)

  WINCONTROL_DECL_DEFAULT(Notebook)

  WINCONTROL_DECL_DEFAULT(Header)

  CONTROL_DECL_DEFAULT(BoundLabel)

  WINCONTROL_DECL_DEFAULT(CustomLabeledEdit)

  WINCONTROL_DECL_DEFAULT(LabeledEdit)

  WINCONTROL_DECL_DEFAULT(CustomColorBox)

  WINCONTROL_DECL_DEFAULT(ColorBox)

// SplitterMouseDownFix fixes a bug in the VCL that causes the splitter to no
// more work with the control in the left/top of it when the control has a size
// of 0. This is actually a TWinControl.AlignControl bug.
procedure SplitterMouseDownFix(Splitter: TSplitter);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

CONTROL_IMPL_DEFAULT(Shape)

CONTROL_IMPL_DEFAULT(PaintBox)

CONTROL_IMPL_DEFAULT(Image)

CONTROL_IMPL_DEFAULT(Bevel)

WINCONTROL_IMPL_DEFAULT(CustomPanel)

WINCONTROL_IMPL_DEFAULT(CustomRadioGroup)

WINCONTROL_IMPL_DEFAULT(CustomControlBar)

WINCONTROL_IMPL_DEFAULT(ControlBar)

WINCONTROL_IMPL_DEFAULT(Panel)

WINCONTROL_IMPL_DEFAULT(RadioGroup)

WINCONTROL_IMPL_DEFAULT(Page)

WINCONTROL_IMPL_DEFAULT(Notebook)

WINCONTROL_IMPL_DEFAULT(Header)

CONTROL_IMPL_DEFAULT(BoundLabel)

WINCONTROL_IMPL_DEFAULT(CustomLabeledEdit)

WINCONTROL_IMPL_DEFAULT(LabeledEdit)

WINCONTROL_IMPL_DEFAULT(CustomColorBox)

WINCONTROL_IMPL_DEFAULT(ColorBox)

CONTROL_IMPL_DEFAULT(Splitter)

procedure TJvExSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.