unit glShade;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, glTypes, glUtils, glCommCl;

type

  TglShade = class(TCustomPanel)
  private
    FImage			: TBitmap;
    fLoaded			: boolean;
    fNeedRebuildImage		: boolean;

  protected
    property Color;//...hide
    procedure Paint; override;
    procedure WMSize( var Msg: TMessage ); message WM_SIZE;
  public
    property Canvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RemakeBackground;//...for users
  published
    property Align;
    property Enabled;
    property Visible;
    property Image: TBitmap read FImage write FImage;
  end;

procedure Register;

implementation
//uses test;
procedure Register;
begin
  RegisterComponents('Proba', [TglShade]);
end;
//*****************************************_____________LowLevel METHODS
//________________________________________________________
constructor TglShade.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 105;
  Height := 105;
  Image:=TBitmap.create;
  fLoaded:=true;
  //...defaults
  fNeedRebuildImage := (csDesigning in ComponentState)and not(csLoading in ComponentState);
end;
//________________________________________________________
destructor TglShade.Destroy;
begin
  Image.Free;
  inherited Destroy;
end;
//________________________________________________________
procedure TglShade.WMSize( var Msg: TMessage );
begin
  if (csDesigning in ComponentState)and not(csLoading in ComponentState) then RemakeBackground;
end;
//________________________________________________________
procedure TglShade.Paint;
var
  i,j,o: integer;
  RGB: COLORREF;
  R,G,B: byte;
const SHIFTCOLOR = $003939;
begin
  if fNeedRebuildImage then
  begin
    Image.Width:=Width; Image.Height:=Height;
    //..prepare tabula rasa :)
    Image.Canvas.Brush.Color:=Parent.Brush.Color;
    Image.Canvas.Brush.Style := bsSolid;
    Image.Canvas.FillRect(ClientRect);
    GetParentImageRect( self, Bounds(Left,Top,Width,Height), Image.Canvas.Handle );
    for j:=0 to Height do
      for i:=0 to Width do
//	if Image.Canvas.Pixels[i,j] > SHIFTCOLOR then
      begin
	if o <> Image.Canvas.Pixels[i,j] then
	begin
	  //o := Image.Canvas.Pixels[i,j];
	  //Form1.Memo1.Lines.Add(Format('%x',[o]));
	end;
//	  if Image.Canvas.Pixels[i,j] = $C8B8A0 then
	RGB := Image.Canvas.Pixels[i,j];
	R := byte( RGB shr 16 );
	G := byte( RGB shr 8 );
	B := byte( RGB );
//	  RShift := $
	Image.Canvas.Pixels[i,j] := Image.Canvas.Pixels[i,j] + SHIFTCOLOR;
      end;
    fNeedRebuildImage := false
  end;

  BitBlt( Canvas.Handle, 0, 0, Width, Height, Image.Canvas.Handle, 0, 0, SRCCOPY);

  if csDesigning in ComponentState then with Canvas do
  begin
    Pen.Color := clBlack; Pen.Style := psDash; Brush.Style := bsClear;
    Rectangle( 0, 0, width, height );
  end;

end;

procedure TglShade.RemakeBackground; //...for users
begin
  fNeedRebuildImage:=true; Repaint;
end;
//________________________________________________________

end.
