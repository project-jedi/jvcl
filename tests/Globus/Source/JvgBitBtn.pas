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

UNIT JvgBitBtn;

INTERFACE
USES
   Windows,
   Messages,
   Classes,
   Controls,
   Graphics,
   JvgTypes,
   JvgCommClasses,
   JvgUtils,
   JvclVer,
   ExtCtrls,
   buttons;
TYPE

   TJvgBitBtn = CLASS(TBitBtn)
   PRIVATE
      FCanvas: TCanvas;
      fMouseEnter: boolean;
    FAboutJVCL: TJVCLAboutInfo;
      PROCEDURE CNDrawItem(VAR Message: TWMDrawItem); MESSAGE CN_DRAWITEM;
      PROCEDURE CMMouseEnter(VAR Message: TMessage); MESSAGE CM_MOUSEENTER;
      PROCEDURE CMMouseLeave(VAR Message: TMessage); MESSAGE CM_MOUSELEAVE;
   PROTECTED
      PROCEDURE DrawItem(CONST DrawItemStruct: TDrawItemStruct);
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
   PUBLISHED
      PROPERTY AboutJVCL: TJVCLAboutInfo READ FAboutJVCL WRITE FAboutJVCL STORED
         False;
   END;

IMPLEMENTATION

//________________________________________________________ Methods _

CONSTRUCTOR TJvgBitBtn.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   FCanvas := TControlCanvas.Create;
   TControlCanvas(FCanvas).Control := Self; //...i can draw now! :)
   //..defaults
END;

DESTRUCTOR TJvgBitBtn.Destroy;
BEGIN
   FCanvas.Free;
   INHERITED;
END;

PROCEDURE TJvgBitBtn.CNDrawItem(VAR Message: TWMDrawItem);
BEGIN
   INHERITED;
   DrawItem(Message.DrawItemStruct^);
END;

PROCEDURE TJvgBitBtn.DrawItem(CONST DrawItemStruct: TDrawItemStruct);
VAR
   IsDown {, IsDefault}       : Boolean;
   //State: TButtonState;
   R                          : TRect;
   BPen, FPen, SPen, OldPen   : HPEN;
   FBrush                     : HBRUSH;
BEGIN
   R := ClientRect;

   WITH DrawItemStruct DO
   BEGIN
      IsDown := itemState AND ODS_SELECTED <> 0;
      //IsDefault := itemState and ODS_FOCUS <> 0;

      {if not Enabled then State := bsDisabled
      else if IsDown then State := bsDown
      else State := bsUp;}
   END;
   R := ClientRect;
   IF (NOT fMouseEnter) AND (NOT IsDown) THEN
   BEGIN

      FBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE));
      IF NOT Focused AND NOT Default THEN
      BEGIN
         SPen := CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNSHADOW));
         FPen := CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNFACE));
         BPen := CreatePen(PS_SOLID, 1, ColorToRGB((Parent AS
            TWinControl).Brush.Color));
         OldPen := SelectObject(DrawItemStruct.hDC, FPen);

         MoveToEx(DrawItemStruct.hDC, R.Left + 1, R.Top + 1, NIL);
         LineTo(DrawItemStruct.hDC, R.Right - 1, R.Top + 1);
         MoveToEx(DrawItemStruct.hDC, R.Left + 1, R.Top + 1, NIL);
         LineTo(DrawItemStruct.hDC, R.Left + 1, R.Bottom - 1);

         SelectObject(DrawItemStruct.hDC, BPen);

         MoveToEx(DrawItemStruct.hDC, R.Left, R.Bottom - 1, NIL);
         LineTo(DrawItemStruct.hDC, R.Right, R.Bottom - 1);
         MoveToEx(DrawItemStruct.hDC, R.Right - 1, R.Top, NIL);
         LineTo(DrawItemStruct.hDC, R.Right - 1, R.Bottom);

         SelectObject(DrawItemStruct.hDC, SPen);

         MoveToEx(DrawItemStruct.hDC, R.Left - 2, R.Bottom - 2, NIL);
         LineTo(DrawItemStruct.hDC, R.Right - 1, R.Bottom - 2);
         MoveToEx(DrawItemStruct.hDC, R.Right - 2, R.Top, NIL);
         LineTo(DrawItemStruct.hDC, R.Right - 2, R.Bottom - 1);

         DeleteObject(SelectObject(DrawItemStruct.hDC, OldPen));
         DeleteObject(FPen);
         DeleteObject(BPen);
      END
      ELSE
      BEGIN
         FrameRect(DrawItemStruct.hDC, Rect(R.Left + 2, R.Top + 2, R.Right - 2,
            R.Bottom - 2), FBrush);
         DeleteObject(FBrush);
      END;
   END;

END;

PROCEDURE TJvgBitBtn.CMMouseEnter(VAR Message: TMessage);
BEGIN
   INHERITED;
   fMouseEnter := true;
   Repaint;
END;

PROCEDURE TJvgBitBtn.CMMouseLeave(VAR Message: TMessage);
BEGIN
   INHERITED;
   fMouseEnter := false;
   Repaint;
END;

END.

