{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgSplit.PAS, released on 2003-01-15.

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

UNIT JvgSplit;

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
   ExtCtrls;
TYPE

   TJvgSplitter = CLASS(TSplitter)
   PRIVATE
      FHotTrack: boolean;
      FTrackCount: integer;
      fActive: boolean;
      FDisplace: boolean;
      PROCEDURE CMMouseEnter(VAR Message: TMessage); MESSAGE CM_MOUSEENTER;
      PROCEDURE CMMouseLeave(VAR Message: TMessage); MESSAGE CM_MOUSELEAVE;
      PROCEDURE WMMouseDblClick(VAR Message: TMessage); MESSAGE
         WM_LBUTTONDBLCLK;
      PROCEDURE SetTrackCount(CONST Value: integer);
      PROCEDURE UpdateControlSize;
      FUNCTION FindControl: TControl;
      PROCEDURE PrepareMarcs(Align: TAlign; VAR pt1, pt2, pt3, pt4, pt5,
         pt6: TPoint);
      PROCEDURE SetDisplace(CONST Value: boolean);
   PROTECTED
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      PROCEDURE Paint; OVERRIDE;
   PUBLISHED
      PROPERTY HotTrack: boolean READ FHotTrack WRITE FHotTrack DEFAULT true;
      PROPERTY TrackCount: integer READ FTrackCount WRITE SetTrackCount DEFAULT
         20;
      PROPERTY Displace: boolean READ FDisplace WRITE SetDisplace DEFAULT true;
   END;

PROCEDURE Register;

IMPLEMENTATION
{~~~~~~~~~~~~~~~~~~~~~~~~~}

PROCEDURE Register;
BEGIN
END;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _

PROCEDURE TJvgSplitter.Paint;
VAR
   i                          : integer;
   sColor                     : TColor;
   pt1, pt2, pt3, pt4, pt5, pt6: TPoint;
   R, R1, R2                  : TRect;
BEGIN
   WITH Canvas DO
   BEGIN

      Brush.Color := self.Color;
      FillRect(ClientRect);

      IF (Align = alBottom) OR (Align = alTop) THEN
      BEGIN
         R1 := classes.Bounds((Width - FTrackCount * 4) DIV 2, 0, 3, 3);
         R2 := classes.Bounds((Width - FTrackCount * 4) DIV 2, 3, 3, 3);
      END
      ELSE
      BEGIN
         R1 := classes.Bounds(0, (Height - FTrackCount * 4) DIV 2, 3, 3);
         R2 := classes.Bounds(3, (Height - FTrackCount * 4) DIV 2, 3, 3);
      END;

      FOR i := 0 TO FTrackCount - 1 DO
      BEGIN
         IF fActive AND HotTrack THEN
            sColor := 0
         ELSE
            sColor := clBtnShadow;

         R := R1;
         Frame3D(Canvas, R, clBtnHighlight, sColor, 1);
         R := R2;
         Frame3D(Canvas, R, clBtnHighlight, sColor, 1);

         IF (Align = alBottom) OR (Align = alTop) THEN
         BEGIN
            OffsetRect(R1, 4, 0);
            OffsetRect(R2, 4, 0);
         END
         ELSE
         BEGIN
            OffsetRect(R1, 0, 4);
            OffsetRect(R2, 0, 4);
         END;

      END;
      IF FDisplace THEN
      BEGIN
         PrepareMarcs(Align, pt1, pt2, pt3, pt4, pt5, pt6);
         IF fActive THEN
            Canvas.Brush.Color := clGray
         ELSE
            Canvas.Brush.Color := clWhite;
         Canvas.Polygon([pt1, pt2, pt3]);
         Canvas.Polygon([pt4, pt5, pt6]);
      END;
   END;
END;

PROCEDURE TJvgSplitter.PrepareMarcs(Align: TAlign; VAR pt1, pt2, pt3, pt4, pt5,
   pt6: TPoint);
BEGIN
   CASE Align OF
      alRight:
         BEGIN
            pt1.x := 1;
            pt1.y := (Height - FTrackCount * 4) DIV 2 - 30;
            pt2.x := 1;
            pt2.y := pt1.y + 6;
            pt3.x := 4;
            pt3.y := pt1.y + 3;

            pt4.x := 1;
            pt4.y := (Height - FTrackCount * 4) DIV 2 + FTrackCount * 4 + 30 -
               7;
            pt5.x := 1;
            pt5.y := pt4.y + 6;
            pt6.x := 4;
            pt6.y := pt4.y + 3;
         END;
      alLeft:
         BEGIN
            pt1.x := 3;
            pt1.y := (Height - FTrackCount * 4) DIV 2 - 30;
            pt2.x := 3;
            pt2.y := pt1.y + 6;
            pt3.x := 0;
            pt3.y := pt1.y + 3;

            pt4.x := 3;
            pt4.y := (Height - FTrackCount * 4) DIV 2 + FTrackCount * 4 + 30 -
               7;
            pt5.x := 3;
            pt5.y := pt4.y + 6;
            pt6.x := 0;
            pt6.y := pt4.y + 3;
         END;
      alTop:
         BEGIN
            pt1.x := (Width - FTrackCount * 4) DIV 2 - 30;
            pt1.y := 4;
            pt2.x := pt1.x + 6;
            pt2.y := 4;
            pt3.x := pt1.x + 3;
            pt3.y := 1;

            pt4.x := (Width - FTrackCount * 4) DIV 2 + FTrackCount * 4 + 30 - 7;
            pt4.y := 4;
            pt5.x := pt4.x + 6;
            pt5.y := 4;
            pt6.x := pt4.x + 3;
            pt6.y := 1;
         END;
      alBottom:
         BEGIN
            pt1.x := (Width - FTrackCount * 4) DIV 2 - 30;
            pt1.y := 1;
            pt2.x := pt1.x + 6;
            pt2.y := 1;
            pt3.x := pt1.x + 3;
            pt3.y := 4;

            pt4.x := (Width - FTrackCount * 4) DIV 2 + FTrackCount * 4 + 30 - 7;
            pt4.y := 1;
            pt5.x := pt4.x + 6;
            pt5.y := 1;
            pt6.x := pt4.x + 3;
            pt6.y := 4;
         END;
   END;
END;

PROCEDURE TJvgSplitter.CMMouseEnter(VAR Message: TMessage);
BEGIN
   INHERITED;
   fActive := true;
   Paint;
END;

PROCEDURE TJvgSplitter.CMMouseLeave(VAR Message: TMessage);
BEGIN
   INHERITED;
   fActive := false;
   Paint;
END;

CONSTRUCTOR TJvgSplitter.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   //..defaults
   Width := 6;
   FHotTrack := true;
   FTrackCount := 20;
   FDisplace := true;
END;

PROCEDURE TJvgSplitter.SetTrackCount(CONST Value: integer);
BEGIN
   FTrackCount := Value;
   Invalidate;
END;

PROCEDURE TJvgSplitter.WMMouseDblClick(VAR Message: TMessage);
BEGIN
   IF FDisplace THEN
      UpdateControlSize;
END;

PROCEDURE TJvgSplitter.UpdateControlSize;
CONST
   FNewSize                   = 0;
   fKeepSize                  : Integer = 0; (* +++ RDB --- *)
VAR
   FControl                   : TControl;
BEGIN
   FControl := FindControl;
   IF NOT Assigned(FControl) THEN
      exit;
   BEGIN
      IF (FKeepSize = 0) THEN
      BEGIN
         CASE Align OF
            alLeft:
               BEGIN
                  FKeepSize := FControl.Width;
                  FControl.Width := FNewSize;

               END;
            alTop:
               BEGIN
                  fKeepSize := FControl.Height;
                  FControl.Height := FNewSize;
               END;
            alRight:
               BEGIN
                  FKeepSize := FControl.Width;
                  Parent.DisableAlign;
                  TRY
                     FControl.Left := FControl.Left + (FControl.Width -
                        FNewSize);
                     FControl.Width := FNewSize;
                  FINALLY
                     Parent.EnableAlign;
                  END;
               END;
            alBottom:
               BEGIN
                  fKeepSize := FControl.Height;
                  Parent.DisableAlign;
                  TRY
                     FControl.Top := FControl.Top + (FControl.Height -
                        FNewSize);
                     FControl.Height := FNewSize;
                  FINALLY
                     Parent.EnableAlign;
                  END;
               END;
         END;
      END
      ELSE (* ++++ RDB +++ *)
      BEGIN
         CASE Align OF
            alLeft:
               BEGIN
                  FControl.Width := FKeepSize;
               END;
            alTop:
               BEGIN
                  FControl.Height := FKeepSize;
               END;
            alRight:
               BEGIN
                  Parent.DisableAlign;
                  TRY
                     FControl.Left := FControl.Left + (FControl.Width -
                        FKeepSize);
                     FControl.Width := FKeepSize;
                  FINALLY
                     Parent.EnableAlign;
                  END;
               END;
            alBottom:
               BEGIN
                  Parent.DisableAlign;
                  TRY
                     FControl.Top := FControl.Top + (FControl.Height -
                        FKeepSize);
                     FControl.Height := FKeepSize;
                  FINALLY
                     Parent.EnableAlign;
                  END;
               END;
         END;
         FKeepSize := 0; (* --- RDB --- *)
      END;
      Update;
      IF Assigned(OnMoved) THEN
         OnMoved(Self);
   END;
END;

FUNCTION TJvgSplitter.FindControl: TControl;
VAR
   P                          : TPoint;
   I                          : Integer;
   R                          : TRect;
BEGIN
   Result := NIL;
   P := Point(Left, Top);
   CASE Align OF
      alLeft: Dec(P.X);
      alRight: Inc(P.X, Width);
      alTop: Dec(P.Y);
      alBottom: Inc(P.Y, Height);
   ELSE
      Exit;
   END;
   FOR I := 0 TO Parent.ControlCount - 1 DO
   BEGIN
      Result := Parent.Controls[I];
      IF Result.Visible AND Result.Enabled THEN
      BEGIN
         R := Result.BoundsRect;
         IF (R.Right - R.Left) = 0 THEN
            IF Align IN [alTop, alLeft] THEN
               Dec(R.Left)
            ELSE
               Inc(R.Right);
         IF (R.Bottom - R.Top) = 0 THEN
            IF Align IN [alTop, alLeft] THEN
               Dec(R.Top)
            ELSE
               Inc(R.Bottom);
         IF PtInRect(R, P) THEN
            Exit;
      END;
   END;
   Result := NIL;
END;

PROCEDURE TJvgSplitter.SetDisplace(CONST Value: boolean);
BEGIN
   FDisplace := Value;
   Invalidate;
END;

END.

