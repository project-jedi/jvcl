{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgAlignFunction.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgAlignFunction;

{$I jvcl.inc}

interface

uses
  Controls,
  JvgTypes;

type
  TJvgNeedAlign = function(Control: TControl): Boolean;

procedure AlignControlsInWindow(Wnd: TWinControl; NeedAlign: TJvgNeedAlign;
  HCAlign: TglHComponentAlign; VCAlign: TglVComponentAlign);

implementation

uses
  Windows, Classes, Math,
  JvgUtils;

procedure AlignControlsInWindow(Wnd: TWinControl; NeedAlign: TJvgNeedAlign;
  HCAlign: TglHComponentAlign; VCAlign: TglVComponentAlign);
var
  I, TotalControls, ControlNo: Integer;
  R: TRect;
  TotalSize, AccumulatedSize: TSize;
  ControlsList: TList;
  Control: TControl;

  procedure Sort(AHorizSort: Boolean);
  var
    I: Integer;
    Sorted: Boolean;
    Control2: TControl;
  begin
    repeat
      Sorted := True;
      for I := 0 to ControlsList.Count - 2 do
      begin
        Control := ControlsList[I];
        Control2 := ControlsList[I + 1];
        if AHorizSort then
        begin
          if Control.Left > Control2.Left then
          begin
            ControlsList.Exchange(I, I + 1);
            Sorted := False;
          end;
        end
        else
        if Control.Top > Control2.Top then
        begin
          ControlsList.Exchange(I, I + 1);
          Sorted := False;
        end;
      end;
    until Sorted;
  end;

begin
  if not Assigned(Wnd) then
    Exit;
  ControlsList := TList.Create;
  try
    R := Rect(MaxInt, MaxInt, 0, 0);
    TotalSize.cx := 0;
    TotalSize.cy := 0;
    AccumulatedSize.cx := 0;
    AccumulatedSize.cy := 0;
    TotalControls := -1;
    ControlNo := 0;
    with Wnd do //...calc sizes and sort controls
      for I := 0 to ControlCount - 1 do
        if NeedAlign(Controls[I]) then
          with Controls[I] do
          begin
            R.Left := Min(R.Left, Left);
            R.Top := Min(R.Top, Top);
            R.Right := Max(R.Right, Left + Width);
            R.Bottom := Max(R.Bottom, Top + Height);
            Inc(TotalSize.cx, Width);
            Inc(TotalSize.cy, Height);
            Inc(TotalControls);
            ControlsList.Add(Controls[I]);
          end;
    Sort(True);

    //..h aligning
    for I := 0 to ControlsList.Count - 1 do
      with Control do
      begin
        Control := ControlsList[I];
        case HCAlign of
          haLeft:
            Left := R.Left;
          haCenters:
            Left := R.Left + (R.Right - R.Left - Width) div 2;
          haRight:
            Left := R.Right - Width;
          haSpaceEqually:
            if ControlNo <> TotalControls then
              Left := R.Left + AccumulatedSize.cx +
                Trunc((R.Right - R.Left - TotalSize.cx) / TotalControls * ControlNo);
          haCenterWindow:
            Left := (Wnd.Width - Width) div 2;
          haClose:
            Left := R.Left + AccumulatedSize.cx;
        end;
        Inc(AccumulatedSize.cx, Width);
        Inc(ControlNo);
      end;
    ControlNo := 0;
    Sort(False);

    //..v aligning
    for I := 0 to ControlsList.Count - 1 do
      with Control do
      begin
        Control := ControlsList[I];
        case VCAlign of
          vaTops:
            Top := R.Top;
          vaCenters:
            Top := R.Top + (R.Bottom - R.Top - Height) div 2;
          vaBottoms:
            Top := R.Bottom - Height;
          vaSpaceEqually:
            if ControlNo <> TotalControls then
              Top := R.Top + AccumulatedSize.cy +
                Trunc((R.Bottom - R.Top - TotalSize.cy) / TotalControls * ControlNo);
          vaCenterWindow:
            Top := (Wnd.Height - Height) div 2;
          vaClose:
            Top := R.Top + AccumulatedSize.cy;
        end;
        Inc(AccumulatedSize.cy, Height);
        Inc(ControlNo);
      end;
  finally
    ControlsList.Free;
  end;
end;

end.

