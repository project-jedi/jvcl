{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgBitBtn.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgBitBtn;

interface
uses
  Windows, Messages, Classes, Controls, Graphics, JvgTypes, JvgCommClasses,
  JvgUtils, ExtCtrls, buttons;
type

  TJvgBitBtn = class(TBitBtn)
  private
    FCanvas: TCanvas;
    fMouseEnter: boolean;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

implementation

//________________________________________________________ Methods _

constructor TJvgBitBtn.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self; //...i can draw now! :)
  //..defaults
end;

destructor TJvgBitBtn.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

procedure TJvgBitBtn.CNDrawItem(var Message: TWMDrawItem);
begin
  inherited;
  DrawItem(Message.DrawItemStruct^);
end;

procedure TJvgBitBtn.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
  IsDown {, IsDefault}: Boolean;
  //State: TButtonState;
  R: TRect;
  BPen, FPen, SPen, OldPen: HPEN;
  FBrush: HBRUSH;
begin
  R := ClientRect;

  with DrawItemStruct do
  begin
    IsDown := itemState and ODS_SELECTED <> 0;
    //IsDefault := itemState and ODS_FOCUS <> 0;

    {if not Enabled then State := bsDisabled
    else if IsDown then State := bsDown
    else State := bsUp;}
  end;
  R := ClientRect;
  if (not fMouseEnter) and (not IsDown) then
  begin

    FBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE));
    if not Focused and not Default then
    begin
      SPen := CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNSHADOW));
      FPen := CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNFACE));
      BPen := CreatePen(PS_SOLID, 1, ColorToRGB((Parent as TWinControl).Brush.Color));
      OldPen := SelectObject(DrawItemStruct.hDC, FPen);

      MoveToEx(DrawItemStruct.hDC, R.Left + 1, R.Top + 1, nil);
      LineTo(DrawItemStruct.hDC, R.Right - 1, R.Top + 1);
      MoveToEx(DrawItemStruct.hDC, R.Left + 1, R.Top + 1, nil);
      LineTo(DrawItemStruct.hDC, R.Left + 1, R.Bottom - 1);

      SelectObject(DrawItemStruct.hDC, BPen);

      MoveToEx(DrawItemStruct.hDC, R.Left, R.Bottom - 1, nil);
      LineTo(DrawItemStruct.hDC, R.Right, R.Bottom - 1);
      MoveToEx(DrawItemStruct.hDC, R.Right - 1, R.Top, nil);
      LineTo(DrawItemStruct.hDC, R.Right - 1, R.Bottom);

      SelectObject(DrawItemStruct.hDC, SPen);

      MoveToEx(DrawItemStruct.hDC, R.Left - 2, R.Bottom - 2, nil);
      LineTo(DrawItemStruct.hDC, R.Right - 1, R.Bottom - 2);
      MoveToEx(DrawItemStruct.hDC, R.Right - 2, R.Top, nil);
      LineTo(DrawItemStruct.hDC, R.Right - 2, R.Bottom - 1);

      DeleteObject(SelectObject(DrawItemStruct.hDC, OldPen));
      DeleteObject(FPen);
      DeleteObject(BPen);
    end
    else
    begin
      FrameRect(DrawItemStruct.hDC, Rect(R.Left + 2, R.Top + 2, R.Right - 2, R.Bottom - 2), FBrush);
      DeleteObject(FBrush);
    end;
  end;

end;

procedure TJvgBitBtn.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  fMouseEnter := true;
  Repaint;
end;

procedure TJvgBitBtn.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  fMouseEnter := false;
  Repaint;
end;

end.
