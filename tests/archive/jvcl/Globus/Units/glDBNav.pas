{ 
  								  
 		 Globus Delphi VCL Extensions Library		   
 			  ' GLOBUS LIB '			   
  			     Freeware				  
  	  Copyright (c) 1998 Chudin A.V, FidoNet: 1246.16	  
  								  
  
 ===================================================================
 glBevel Unit 08.1998				  component TglBevel

 This unit implements the TglBevel component which is an  extended
 TBevel Delphi component with gradient filling and advanced  borders
 drawing.
 ===================================================================
}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//...created: 08.1998
//...last modified: 26.11.1998
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unit glDBNav;

interface
{$I glDEF.INC}
uses
  Windows, Messages, Classes, Controls, Graphics, glTypes, glCommCl,
  glUtils, DBCtrls;
type

  TglDBNAvigator = class(TDBNAvigator)
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
  RegisterComponents('Proba', [TglDBNAvigator]);
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _
constructor TglDBNAvigator.Create( AOwner : TComponent );
begin
  inherited;
//  FocusControl := Controls[0];
//  HookFocusControlWndProc;
end;

destructor TglDBNAvigator.Destroy;
begin
//  UnhookFocusControlWndProc;
  inherited;
end;

procedure TglDBNAvigator.WMPaint(var Message: TWMPaint);
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

procedure TglDBNAvigator.PaintControls(DC: HDC; First: TControl);
begin
  if DC <> 0 then FNewWndProc := nil;
end;

procedure TglDBNAvigator.HookFocusControlWndProc;
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
procedure TglDBNAvigator.UnhookFocusControlWndProc;
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
procedure TglDBNAvigator.FocusControlWndHookProc(var Msg_: TMessage);
begin
{  case Msg_.Msg of
    WM_PAINT:
    begin Msg_.Result := 1; exit; end;
  end;
  with Msg_ do Result := CallWindowProc( FPrevWndProc, TForm(Owner).Handle, Msg, WParam, LParam );}
end;
//______

end.
