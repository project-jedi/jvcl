unit JvDotNetUtils;

interface
uses
  Windows, Messages, Classes, SysUtils, Graphics, Controls;

procedure DotNetMessageHandler(var Message:TMessage; AControl: TWinControl; AColor:TColor; var InControl: Boolean);
procedure DrawDotNetControl(Control: TWinControl; AColor:TColor; InControl: Boolean);

implementation

procedure DotNetMessageHandler(var Message:TMessage; AControl: TWinControl; AColor:TColor; var InControl: Boolean);
begin
  if Message.Msg = CM_MOUSEENTER then
    InControl := True;
  if Message.Msg = CM_MOUSELEAVE then
    InControl := False;
  case Message.Msg of
    CM_MOUSEENTER, CM_MOUSELEAVE, WM_SETFOCUS, WM_KILLFOCUS, WM_NCPAINT:
      DrawDotNetControl(AControl, AColor, InControl);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: DrawDotNetControl
  Author:    mh
  Date:      25-Jun-2002
  Arguments: Control: TJvNetControl; AColor:TColor; InControl: Boolean
  Result:    None
-----------------------------------------------------------------------------}

procedure DrawDotNetControl(Control: TWinControl; AColor:TColor; InControl: Boolean);
var
  DC: HDC;
  R: TRect;
  Canvas: TCanvas;
begin
  DC := GetWindowDC(Control.Handle);
  try
    GetWindowRect(Control.Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    Canvas := TCanvas.Create;
    with Canvas do
    try
      Handle := DC;
      Brush.Color := clGray;
      if Control.Focused or InControl then
        Brush.Color := $00733800;
      FrameRect(R);
      InflateRect(R, -1, -1);
      if not (Control.Focused or InControl) then
        Brush.Color := AColor;
      FrameRect(R);
    finally // wrap up
      Free;
    end; // try/finally
  finally // wrap up
    ReleaseDC(Control.Handle, DC);
  end; // try/finally
end;

end.
