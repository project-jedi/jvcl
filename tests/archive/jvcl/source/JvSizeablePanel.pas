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
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  JvPanel;

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
  end;

implementation

procedure TJvSizeablePanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and ((Width - X) < 10) and ((Height - Y) < 10) then
  begin
    FDragging := TRue;
    FLastPos := Point(X, Y);
    MouseCapture := True;
    Screen.cursor := crSizeNWSE;
  end
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvSizeablePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  if FDragging then
  begin
    R := BoundsRect;
    SetBounds(R.Left, R.Top,
      R.Right - R.Left + X - FLastPos.X,
      R.Bottom - R.Top + Y - FLastPos.Y);
    FLastPos := Point(X, Y);
  end
  else
  begin
    inherited MouseMove(Shift, X, Y);
    if ((Width - X) < 10) and ((Height - Y) < 10) then
      Cursor := crSizeNWSE
    else
      Cursor := crDefault;
  end;
end;

procedure TJvSizeablePanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FDragging then
  begin
    FDragging := False;
    MouseCapture := False;
    Screen.Cursor := crDefault;
  end
  else
    inherited MouseUp(Button, Shift, X, Y);
end;

procedure TJvSizeablePanel.Paint;
var
  X, Y: Integer;
begin
  inherited Paint;
  with Canvas do
  begin
    Font.Name := 'Marlett';
    Font.Charset := DEFAULT_CHARSET;
    Font.Size := 10;
    if fsBold in Canvas.Font.Style then
      Canvas.Font.Style := Canvas.Font.Style - [fsBold];
    if fsItalic in Canvas.Font.Style then
      Canvas.Font.Style := Canvas.Font.Style - [fsItalic];
    Brush.Style := bsClear;
    X := ClientWidth - Canvas.TextWidth('o');
    Y := ClientHeight - Canvas.TextWidth('o');
    TextOut(X, Y, 'o');
  end;
end;

end.

