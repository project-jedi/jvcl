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

Contributor(s): Michael Beck [mbeck1@compuserve.com]

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSizeablePanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, JvPanel;

type
  TJvSizeablePanel = class(TJvPanel)
  private
    FDragging: Boolean;
    FLastPos: TPoint;

  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    { Public declarations }
  published
    { Published declarations }
  end;

implementation

procedure TJvSizeablePanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if (Button = mbLeft) and
    ((Width - x) < 10) and
    ((Height - y) < 10) then
  begin
    FDragging := TRue;
    FLastPos := Point(x, y);
    MouseCapture := true;
    Screen.cursor := crSizeNWSE;
  end
  else
    inherited;
end;

procedure TJvSizeablePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  r: TRect;
begin
  if FDragging then
  begin
    r := BoundsRect;
    SetBounds(r.left, r.top,
      r.right - r.left + X - FlastPos.X,
      r.bottom - r.top + Y - Flastpos.Y);
    FLastPos := Point(x, y);
  end
  else
  begin
    inherited;
    if ((Width - x) < 10) and ((Height - y) < 10) then
      Cursor := crSizeNWSE
    else
      Cursor := crDefault;
  end;
end;

procedure TJvSizeablePanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if FDragging then
  begin
    FDragging := False;
    MouseCapture := false;
    Screen.Cursor := crDefault;
  end
  else
    inherited;
end;

procedure TJvSizeablePanel.Paint;
var
  x, y: Integer;
begin
  inherited;
  with Canvas do
  begin
    Font.Name := 'Marlett';
    Font.Charset := default_Charset;
    Font.Size := 10;
    if fsBold in Canvas.Font.Style then Canvas.Font.Style := Canvas.Font.Style - [fsBold];
    if fsItalic in Canvas.Font.Style then Canvas.Font.Style := Canvas.Font.Style - [fsItalic];
    Brush.Style := bsClear;
    x := clientwidth - canvas.textwidth('o');
    y := clientheight - canvas.textheight('o');
    textout(x, y, 'o');
  end;
end;
end.

