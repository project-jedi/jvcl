{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgBevel.PAS, released on 2003-01-15.

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

{  This unit implements the TJvgBevel component which is an  extended
 TBevel Delphi component with gradient filling and advanced  borders
 drawing.}

UNIT JvgDBNav;

INTERFACE
USES
   Windows,
   Messages,
   Classes,
   JVCLVer,
   Controls,
   Graphics,
   JvgTypes,
   JvgCommClasses,
   JvgUtils,
   DBCtrls;
TYPE

   TJvgDBNAvigator = CLASS(TDBNAvigator)
   PRIVATE
      FAboutJVCL: TJVCLAboutInfo;
      FPrevWndProc: Pointer;
      FNewWndProc: Pointer;
      FocusControl: TWinControl;
      //    procedure CMMouseMove(var Message: TMessage); message CM_MOUSEMOVE;
      //    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
      PROCEDURE WMPaint(VAR Message: TWMPaint); MESSAGE WM_PAINT;
      PROCEDURE PaintControls(DC: HDC; First: TControl);
      PROCEDURE HookFocusControlWndProc;
      PROCEDURE UnhookFocusControlWndProc;
      PROCEDURE FocusControlWndHookProc(VAR Msg_: TMessage);
   PROTECTED
      //    procedure PaintHandler(var Message: TWMPaint);
      //    procedure Paint; override;
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
   PUBLISHED
      PROPERTY AboutJVCL: TJVCLAboutInfo READ FAboutJVCL WRITE FAboutJVCL STORED
         False;
   END;

IMPLEMENTATION
//________________________________________________________ Methods _

CONSTRUCTOR TJvgDBNAvigator.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   //  FocusControl := Controls[0];
   //  HookFocusControlWndProc;
END;

DESTRUCTOR TJvgDBNAvigator.Destroy;
BEGIN
   //  UnhookFocusControlWndProc;
   INHERITED;
END;

PROCEDURE TJvgDBNAvigator.WMPaint(VAR Message: TWMPaint);
VAR
   DC                         : HDC;
   i                          : integer;
   PS                         : TPaintStruct;
BEGIN
   DC := BeginPaint(Handle, PS);
   EndPaint(Handle, PS);
   exit;
   DC := Message.DC;
   IF DC = 0 THEN
      DC := BeginPaint(Handle, PS);
   TRY
      //PaintWindow(DC);
      FOR i := 0 TO ControlCount - 1 DO
         IF Controls[i] IS TNavButton THEN
            WITH TNavButton(Controls[i]) DO
               IF Visible AND Enabled THEN
                  //        BitBlt( DC, Width*I,1,{Glyph.width,Glyph.Height}10,10,Glyph.Canvas.Handle, 0,0, SRCCOPY);

                  SetPixel(DC, 2, 2, 0);
      //    BitBlt( DC, 0,0,10,10,TNavButton(Controls[0]).Glyph.Canvas.Handle,0,0, SRCCOPY);
      BEGIN

      END;
   FINALLY
      IF Message.DC = 0 THEN
         EndPaint(Handle, PS);
   END;
END;

PROCEDURE TJvgDBNAvigator.PaintControls(DC: HDC; First: TControl);
BEGIN
   IF DC <> 0 THEN
      FNewWndProc := NIL;
END;

PROCEDURE TJvgDBNAvigator.HookFocusControlWndProc;
VAR
   P                          : Pointer;
BEGIN
   {  P := Pointer(GetWindowLong( FocusControl.Handle, GWL_WNDPROC));
     if (P <> FNewWndProc) then begin
       FPrevWndProc := P;
       FNewWndProc := MakeObjectInstance( FocusControlWndHookProc );
       SetWindowLong( FocusControl.Handle, GWL_WNDPROC, LongInt(FNewWndProc));
     end;}
END;
//______

PROCEDURE TJvgDBNAvigator.UnhookFocusControlWndProc;
BEGIN
   //  if not(csDesigning in ComponentState) then exit;
   {  if (FNewWndProc<>nil)and(FPrevWndProc<>nil)
       and(Pointer(GetWindowLong( FocusControl.Handle, GWL_WNDPROC)) = FNewWndProc) then
     begin
       SetWindowLong( FocusControl.Handle, GWL_WNDPROC, LongInt(FPrevWndProc));
       FNewWndProc:=nil;
     end;}
END;
//______

PROCEDURE TJvgDBNAvigator.FocusControlWndHookProc(VAR Msg_: TMessage);
BEGIN
   {  case Msg_.Msg of
       WM_PAINT:
       begin Msg_.Result := 1; exit; end;
     end;
     with Msg_ do Result := CallWindowProc( FPrevWndProc, TForm(Owner).Handle, Msg, WParam, LParam );}
END;
//______

END.

