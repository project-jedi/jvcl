// a unit to demonstrate how to put your own button in the caption bar of the program
unit Capbtn;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Buttons;

type
  TTitleBtnFrm = class(TForm)
    procedure FormResize(Sender: TObject);
  private
    TitleButton : TRect;
    procedure DrawTitleButton;
    {Paint-related messages}
    procedure WMSetText(var Msg : TWMSetText); message WM_SETTEXT;
    procedure WMNCPaint(var Msg : TWMNCPaint); message WM_NCPAINT;
    procedure WMNCActivate(var Msg : TWMNCActivate); message WM_NCACTIVATE;
    {Mouse down-related messages}
    procedure WMNCHitTest(var Msg : TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Msg : TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    function GetVerInfo : DWORD;
  end;

var
  TitleBtnForm: TTitleBtnFrm;

const
  htTitleBtn = htSizeLast + 1;

implementation
{$R *.DFM}

procedure TTitleBtnFrm.DrawTitleButton;
var
  bmap : TBitmap; {Bitmap to be drawn - 16 X 16 : 16 Colors}
  XFrame,  {X and Y size of Sizeable area of Frame}
  YFrame,
  XTtlBit, {X and Y size of Bitmaps in caption}
  YTtlBit  : Integer;
begin
  {Get size of form frame and bitmaps in title bar}
  XFrame  := GetSystemMetrics(SM_CXFRAME);
  YFrame  := GetSystemMetrics(SM_CYFRAME);
  XTtlBit := GetSystemMetrics(SM_CXSIZE);
  YTtlBit := GetSystemMetrics(SM_CYSIZE);

  // get coordinates to place the button
  TitleButton := Bounds(Width - XFrame - 4 * XTtlBit,
                           XFrame+2,
                           XTtlBit -1,
                           YTtlBit -4);

  Canvas.Handle := GetWindowDC(Self.Handle); {Get Device context for drawing}
  try
    {Draw a button face on the TRect}
    DrawButtonFace(Canvas, TitleButton, 1, bsAutoDetect, False, False, False);
    bmap := TBitmap.Create;
    bmap.LoadFromFile('help.bmp');
    // add the bitmap (should be small - about 12x12)
  with TitleButton do
       Canvas.Draw(Width - XFrame - 4 * XTtlBit + 2,XFrame+4, bmap);

  finally
    ReleaseDC(Self.Handle, Canvas.Handle);
    bmap.Free;
    Canvas.Handle := 0;
  end;
end;

{Paint triggering events}
procedure TTitleBtnFrm.WMNCActivate(var Msg : TWMNCActivate);
begin
  Inherited;
  DrawTitleButton;
end;

procedure TTitleBtnFrm.FormResize(Sender: TObject);
begin
  Perform(WM_NCACTIVATE, Word(Active), 0);
end;

{Painting events}
procedure TTitleBtnFrm.WMNCPaint(var Msg : TWMNCPaint);
begin
  Inherited;
  DrawTitleButton;
end;

procedure TTitleBtnFrm.WMSetText(var Msg : TWMSetText);
begin
  Inherited;
  DrawTitleButton;
end;

{Mouse-related procedures}
procedure TTitleBtnFrm.WMNCHitTest(var Msg : TWMNCHitTest);
begin
  Inherited;
  {Check to see if the mouse was clicked in the area of the button}
  with Msg do
    if PtInRect(TitleButton, Point(XPos - Left, YPos - Top)) then
      Result := htTitleBtn;
end;

procedure TTitleBtnFrm.WMNCLButtonDown(var Msg : TWMNCLButtonDown);
begin
  inherited;
  if (Msg.HitTest = htTitleBtn) then
    ShowMessage('You pressed the new button');
end;

function TTitleBtnFrm.GetVerInfo : DWORD;
var
 verInfo : TOSVERSIONINFO;
begin
  verInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(verInfo) then
    Result := verInfo.dwPlatformID;
    {Returns:
      VER_PLATFORM_WIN32s	      Win32s on Windows 3.1
      VER_PLATFORM_WIN32_WINDOWS	Win32 on Windows 95
      VER_PLATFORM_WIN32_NT	      Windows NT }
end;


end.
