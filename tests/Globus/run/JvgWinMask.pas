{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgWinMask.PAS, released on 2003-01-15.

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

UNIT JvgWinMask;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   Classes,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   ComCtrls,
   extctrls,
   JvgTypes,
   JvComponent,
   CommCtrl,
   JvgCommClasses
   {$IFDEF COMPILER5_UP},
   Imglist{$ENDIF};

//const

TYPE

   TJvgWinMask = CLASS(TJvCustomPanel)
   PRIVATE
      FMask: TBitmap;
      FMaskBuff: TBitmap;
      fIgnorePaint: boolean;
   PUBLIC
      Control: TWinControl;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
   PROTECTED
      PROCEDURE Loaded; OVERRIDE;
      PROCEDURE Paint; OVERRIDE;
      PROCEDURE SetParent(Value: TWinControl); OVERRIDE;
      PROCEDURE Notification(AComponent: TComponent; Operation: TOperation);
         OVERRIDE;
   PUBLIC
      PROPERTY Mask: TBitmap READ FMask WRITE FMask;  // stored fDontUseDefaultImage;
   END;

PROCEDURE Register;

IMPLEMENTATION
USES JvgUtils;
{~~~~~~~~~~~~~~~~~~~~~~~~~}

PROCEDURE Register;
BEGIN
END;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//___________________________________________________ TJvgWinMask Methods _

CONSTRUCTOR TJvgWinMask.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   Height := 50;
   Width := 100;
   FMask := TBitmap.Create;
   FMaskBuff := TBitmap.Create;
   fIgnorePaint := false;
END;

DESTRUCTOR TJvgWinMask.Destroy;
BEGIN
   FMask.Free;
   FMaskBuff.Free;
   INHERITED;
END;

PROCEDURE TJvgWinMask.Loaded;
BEGIN
   INHERITED;
END;

PROCEDURE TJvgWinMask.Paint;
VAR
   r                          : TRect;
   CurrStyle                  : TJvgTextBoxStyle;
   OldPointer                 : Pointer;
   Message                    : TMessage;

   PROCEDURE CreateMaskBuff(R: TRect);
   BEGIN
      FMaskBuff.Width := Width;
      FMaskBuff.Height := Height;

      FMaskBuff.Canvas.Brush.Color := clBlue;
      FMaskBuff.Canvas.FillRect(R);

      Message.Msg := WM_PAINT;
      SendMessage(Control.Handle, WM_PAINT, FMaskBuff.Canvas.handle, 0);
      //    GetWindowImageFrom(Control, 0, 0, true, false, FMaskBuff.Canvas.handle);
      //    GetParentImageRect( self, Bounds(Left,Top,Width,Height),
      //			  FMaskBuff.Canvas.Handle );

      //    BitBlt( FMaskBuff.Canvas.Handle, 0, 0, Width, Height,
      //            FMask.Canvas.Handle, 0, 0, SRCPAINT );

      BitBlt(Canvas.Handle, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
         FMaskBuff.Canvas.Handle, 0, 0, SRCCOPY);
      //    FMaskBuff
   END;
BEGIN
   IF fIgnorePaint THEN
      exit;
   fIgnorePaint := true;

   r := ClientRect;
   IF Enabled THEN
   BEGIN
      CreateMaskBuff(R);

      //    BitBlt( Canvas.Handle, R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top,
      //            FMaskBuff.Canvas.Handle, 0, 0, SRCCOPY );
   END;
   //  if Assigned(FAfterPaint) then FAfterPaint(self);
   fIgnorePaint := false;
END;

PROCEDURE TJvgWinMask.SetParent(Value: TWinControl);
BEGIN
   INHERITED;
END;

PROCEDURE TJvgWinMask.Notification(AComponent: TComponent; Operation:
   TOperation);
BEGIN
   INHERITED Notification(AComponent, Operation);
END;

END.

