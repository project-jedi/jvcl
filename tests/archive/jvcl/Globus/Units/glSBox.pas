//...created: 30:03:99
//...last modified: 30:03:99
unit glSBox;
{$I glDEF.INC}
interface
uses
  Windows, Messages, Classes, Controls, Graphics, glTypes, glCommCl,
  glUtils, Forms, OleCtnrs, ExtCtrls, SysUtils;
type
   TOnEraseBkgndEvent = procedure (Sender: TObject; DC: HDC ) of object;

  TglScrollBox = class(TScrollBox)
  private
    FBack: TBitmap;
    Buffer: TBitmap;
    FOnEraseBkgndEvent: TOnEraseBkgndEvent;
    procedure WMEraseBkgnd( var Msg: TWMEraseBkgnd ); message WM_ERASEBKGND;
    function GetBack: TBitmap;
    procedure SetBack(Value: TBitmap);
    procedure SetOnEraseBkgndEvent(const Value: TOnEraseBkgndEvent);
  public
    BufferedDraw: boolean;
    procedure ApplyBuffer(DC: HDC);
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
  published
    property Background:TBitmap read GetBack write SetBack;
    property OnEraseBkgndEvent: TOnEraseBkgndEvent read FOnEraseBkgndEvent write SetOnEraseBkgndEvent;
  end;

  procedure Register;
implementation

{~~~~~~~~~~~~~~~~~~~~~~~~~}
procedure Register;
begin
  RegisterComponents('Proba', [TglScrollBox]);
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _

constructor TglScrollBox.Create( AOwner : TComponent );
begin
  inherited Create( AOwner );
{  FBack := TBitmap.Create;
  FBack.Width := 8; FBack.Height := 8;
  FBack.Canvas.Brush.Color := clWhite;//clWindow;
  FBack.Canvas.FillRect( Rect(0,0,8,8) );
  FBack.Canvas.Pixels[7,7] := 0;}
end;

destructor TglScrollBox.Destroy;
begin
  if Assigned(FBack) then FBack.Free;
  if Assigned(Buffer) then Buffer.Free;
  inherited;
end;

procedure TglScrollBox.WMEraseBkgnd( var Msg: TWMEraseBkgnd );
var
  DC: HDC;
  r: TRect;
  IHeight, IWidth, SavedIHeight,x_,y_,XOffset,YOffset,SavedYOffset: integer;
  Canvas: TCanvas;
begin
  Canvas := nil;

  if BufferedDraw and (Buffer = nil) then Buffer := TBitmap.Create;

  if assigned(Buffer) then
  begin
   Buffer.Width := Width;
   Buffer.Height := Height;
  end;

  if BufferedDraw then DC := Buffer.Canvas.Handle else DC := Msg.DC;

  try

  if not Assigned(FBack) then exit;


  if FBack.Width <= 8 then
  with Canvas do
  begin
    Canvas := TCanvas.Create;
    Handle := Msg.DC;
//    Pen.Color := clWindow;
//    Brush.Color := clWindow;
//    Brush.Style := bsCross;
    Brush.Bitmap:= FBack;
    FillRect( ClientRect );
    Handle := 0;
    Msg.Result := 1;
  end else
  begin
//  Sendmessage(self.Handle, WM_SETREDRAW, 0, 0);
//    BitBlt( Msg.DC, x_, y_, 100, 100, FBack.canvas.Handle, 0, 0, SRCCOPY);
    r := ClientRect;
    x_ := r.left; y_ := r.top;
    IHeight := FBack.Height;
    IWidth := FBack.Width;
    SavedIHeight:=IHeight;

    XOffset := HorzScrollBar.Position - trunc(HorzScrollBar.Position / IWidth)*IWidth;
    YOffset := VertScrollBar.Position - trunc(VertScrollBar.Position / IHeight)*IHeight;
    SavedYOffset := YOffset;
    while x_ < r.right do
    begin
      //if x_+IWidth > r.right then IWidth := r.right-x_;
      while y_ < r.bottom do begin
        IHeight:=SavedIHeight;
        //if y_+IHeight-YOffset > r.bottom then IHeight := r.bottom-y_;
     	BitBlt( DC, x_, y_, IWidth-XOffset, IHeight-YOffset, FBack.canvas.Handle, XOffset, YOffset, SRCCOPY);
        Inc(y_, IHeight-YOffset); YOffset := 0;
      end;
      Inc(x_, IWidth-XOffset); y_:=r.top;  XOffset := 0;
      YOffset := SavedYOffset;
    end;

  end;

  finally
    if Assigned(OnEraseBkgndEvent) then OnEraseBkgndEvent(self, DC);
    if Assigned(Canvas) then Canvas.Free;
    if BufferedDraw then ApplyBuffer(Msg.DC);
  end;
end;


//===========================================================================

function TglScrollBox.GetBack: TBitmap;
begin
  if not Assigned(FBack) then FBack := TBitmap.Create;
  Result := FBack;
end;

procedure TglScrollBox.SetBack(Value: TBitmap);
begin
  if Assigned(FBack) then FBack.Free;
  FBack := TBitmap.Create;
  FBack.Assign( Value ); Invalidate;
end;

procedure TglScrollBox.SetOnEraseBkgndEvent(const Value: TOnEraseBkgndEvent);
begin
  FOnEraseBkgndEvent := Value;
end;

procedure TglScrollBox.ApplyBuffer(DC: HDC);
begin
  BitBlt( DC, 0, 0, Width, Height, Buffer.Canvas.Handle, 0, 0, SRCCOPY);
end;

end.
