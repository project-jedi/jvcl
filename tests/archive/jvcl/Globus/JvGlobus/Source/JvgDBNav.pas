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

unit JvgDBNav;

interface
{$I glDEF.INC}
uses
  Windows, Messages, Classes, Controls, Graphics, JvgTypes, JvgCommClasses,
  JvgUtils, DBCtrls;
type

  TJvgDBNAvigator = class(TDBNAvigator)
  private
    FPrevWndProc	: Pointer;
    FNewWndProc 	: Pointer;
    FocusControl        : TWinControl;
//    procedure CMMouseMove(var Message: TMessage); message CM_MOUSEMOVE;
//    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure PaintControls(DC: HDC; First: TControl);
    procedure HookFocusControlWndProc;
    procedure UnhookFocusControlWndProc;
    procedure FocusControlWndHookProc(var Msg_: TMessage);
  protected
//    procedure PaintHandler(var Message: TWMPaint);
//    procedure Paint; override;
  public
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
  published
  end;

procedure Register;

implementation
{~~~~~~~~~~~~~~~~~~~~~~~~~}
procedure Register;
begin
  RegisterComponents('Proba', [TJvgDBNAvigator]);
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _
constructor TJvgDBNAvigator.Create( AOwner : TComponent );
begin
  inherited;
//  FocusControl := Controls[0];
//  HookFocusControlWndProc;
end;

destructor TJvgDBNAvigator.Destroy;
begin
//  UnhookFocusControlWndProc;
  inherited;
end;

procedure TJvgDBNAvigator.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  i: integer;
  PS: TPaintStruct;
begin
DC := BeginPaint(Handle, PS);
EndPaint(Handle, PS);
exit;
  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  try
//PaintWindow(DC);
    for i := 0 to ControlCount - 1 do if Controls[i] is TNavButton then
      with TNavButton(Controls[i]) do if Visible and Enabled then
//        BitBlt( DC, Width*I,1,{Glyph.width,Glyph.Height}10,10,Glyph.Canvas.Handle, 0,0, SRCCOPY);

    SetPixel(DC,2,2,0);
//    BitBlt( DC, 0,0,10,10,TNavButton(Controls[0]).Glyph.Canvas.Handle,0,0, SRCCOPY);
   begin

   end;
  finally
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;
end;

procedure TJvgDBNAvigator.PaintControls(DC: HDC; First: TControl);
begin
  if DC <> 0 then FNewWndProc := nil;
end;

procedure TJvgDBNAvigator.HookFocusControlWndProc;
var
  P:Pointer;
begin
{  P := Pointer(GetWindowLong( FocusControl.Handle, GWL_WNDPROC));
  if (P <> FNewWndProc) then begin
    FPrevWndProc := P;
    FNewWndProc := MakeObjectInstance( FocusControlWndHookProc );
    SetWindowLong( FocusControl.Handle, GWL_WNDPROC, LongInt(FNewWndProc));
  end;}
end;
//______
procedure TJvgDBNAvigator.UnhookFocusControlWndProc;
begin
//  if not(csDesigning in ComponentState) then exit;
{  if (FNewWndProc<>nil)and(FPrevWndProc<>nil)
    and(Pointer(GetWindowLong( FocusControl.Handle, GWL_WNDPROC)) = FNewWndProc) then
  begin
    SetWindowLong( FocusControl.Handle, GWL_WNDPROC, LongInt(FPrevWndProc));
    FNewWndProc:=nil;
  end;}
end;
//______
procedure TJvgDBNAvigator.FocusControlWndHookProc(var Msg_: TMessage);
begin
{  case Msg_.Msg of
    WM_PAINT:
    begin Msg_.Result := 1; exit; end;
  end;
  with Msg_ do Result := CallWindowProc( FPrevWndProc, TForm(Owner).Handle, Msg, WParam, LParam );}
end;
//______

end.
