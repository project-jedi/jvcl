{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgBevel.PAS, released on 2003-01-15.

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

unit JvgDBNav;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Classes, Controls, Graphics, DBCtrls,
  {$IFDEF USEJVCL}
  JVCLVer,
  {$ENDIF USEJVCL}
  JvgTypes, JvgCommClasses, JvgUtils;

type
  TJvgDBNavigator = class(TDBNavigator)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    //      FNewWndProc: Pointer;
    //    procedure CMMouseMove(var Message: TMessage); message CM_MOUSEMOVE;
    //    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    //      procedure PaintControls(DC: HDC; First: TControl);
    //      procedure HookFocusControlWndProc;
    //      procedure UnhookFocusControlWndProc;
    //      procedure FocusControlWndHookProc(var Msg_: TMessage);
  protected
    //    procedure PaintHandler(var message: TWMPaint);
    //    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    {$IFDEF USEJVCL}
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    {$ENDIF USEJVCL}
  end;

implementation

{$IFDEF USEJVCL}
uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvThemes;
{$ENDIF USEJVCL}

constructor TJvgDBNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF USEJVCL}
  IncludeThemeStyle(Self, [csParentBackground]);
  {$ENDIF USEJVCL}
  //  FocusControl := Controls[0];
  //  HookFocusControlWndProc;
end;

destructor TJvgDBNavigator.Destroy;
begin
  //  UnhookFocusControlWndProc;
  inherited Destroy;
end;

procedure TJvgDBNavigator.WMPaint(var Msg: TWMPaint);
var
  DC: HDC;
  I: Integer;
  PS: TPaintStruct;
begin
  //   DC := BeginPaint(Handle, PS);
  EndPaint(Handle, PS);
  Exit;

  DC := Msg.DC;
  if DC = 0 then
    DC := BeginPaint(Handle, PS);
  try
    //PaintWindow(DC);
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TNavButton then
        with TNavButton(Controls[I]) do
          if Visible and Enabled then
            //        BitBlt( DC, Width*I,1,{Glyph.Width,Glyph.Height}10,10,Glyph.Canvas.Handle, 0,0, SRCCOPY);

            SetPixel(DC, 2, 2, 0);
    //    BitBlt( DC, 0,0,10,10,TNavButton(Controls[0]).Glyph.Canvas.Handle,0,0, SRCCOPY);
  finally
    if Msg.DC = 0 then
      EndPaint(Handle, PS);
  end;
end;

{
procedure TJvgDBNavigator.PaintControls(DC: HDC; First: TControl);
begin
   if DC <> 0 then
      FNewWndProc := NIL;
end;
}
{
procedure TJvgDBNavigator.HookFocusControlWndProc;
begin
     P := Pointer(GetWindowLong( FocusControl.Handle, GWL_WNDPROC));
     if (P <> FNewWndProc) then
     begin
       FPrevWndProc := P;
       FNewWndProc := MakeObjectInstance( FocusControlWndHookProc );
       SetWindowLong( FocusControl.Handle, GWL_WNDPROC, LongInt(FNewWndProc));
     end;
end;
}
//______
{
procedure TJvgDBNavigator.UnhookFocusControlWndProc;
begin
   //  if not(csDesigning in ComponentState) then exit;
     if (FNewWndProc<>nil)and(FPrevWndProc<>nil)
       and(Pointer(GetWindowLong( FocusControl.Handle, GWL_WNDPROC)) = FNewWndProc) then
     begin
       SetWindowLong( FocusControl.Handle, GWL_WNDPROC, LongInt(FPrevWndProc));
       FNewWndProc:=nil;
     end;
end;
//______
}

{
procedure TJvgDBNavigator.FocusControlWndHookProc(var Msg_: TMessage);
begin
     case Msg_.Msg of
       WM_PAINT:
       begin Msg_.Result := 1; exit; end;
     end;
     with Msg_ do Result := CallWindowProc( FPrevWndProc, TForm(Owner).Handle, Msg, WParam, LParam );
end;
//______
}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

