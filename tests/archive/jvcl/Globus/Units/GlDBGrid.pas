unit GlDBGrid;

interface
uses
  Windows, Messages, Classes, Controls, Graphics, glTypes, glCommCl,
  glUtils, ExtCtrls, grids, dbgrids;

type

  TglDBGrid = class(TDBGrid)
  private
    FAlignment          : TAlignment;
    FAutoColumnSize     : boolean;
    FCaptionHeight      : integer;
    FBitmap,bmp	        : TBitmap;
    FImage              : TImage;
    FGlyphs		: TImageList;
    FSingleGlyph	: boolean;

    GlyphsChangeLink	: TChangeLink;
    Glyph               : TBitmap;
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaptionHeight(Value: integer);
    function  GetBitmap: TBitmap;
    procedure SetBitmap(Value: TBitmap);
    procedure SetImage(Value: TImage);
    procedure SetGlyphs(Value: TImageList);
    procedure SetSingleGlyph(Value: boolean);
  protected
    procedure Loaded; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
     procedure GlyphsListChanged(Sender: TObject);
  public
    AlignAll    : boolean;
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
  published
    property CaptionTextAlignment  : TAlignment read FAlignment write SetAlignment
     default taCenter;
    property CaptionHeight: integer read FCaptionHeight write SetCaptionHeight
     default 17;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Image: TImage read FImage write SetImage;
    property Glyphs: TImageList read FGlyphs write SetGlyphs;
    property SingleGlyph: boolean read FSingleGlyph write SetSingleGlyph
       default false;
  end;

implementation

constructor TglDBGrid.Create( AOwner : TComponent );
begin
  inherited;
  FAlignment            := taCenter;
  FCaptionHeight        := 17;
  FSingleGlyph          := false;
  Glyph                 := TBitmap.Create;
  GlyphsChangeLink	:= TChangeLink.Create;
  GlyphsChangeLink.OnChange := GlyphsListChanged;
end;

destructor TglDBGrid.Destroy;
begin
  if Assigned(FBitmap) then FBitmap.Free;
  GlyphsChangeLink.Free;
  Glyph.Free;
  inherited;
end;

procedure TglDBGrid.Loaded;
begin
  inherited;
  if Assigned(FBitmap)and(not FBitmap.Empty) then Bmp := FBitmap;
  RowHeights[0] := FCaptionHeight;
end;

procedure TglDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
const
  aAlignments: array[TAlignment] of Longint = ( ES_LEFT, ES_RIGHT, ES_CENTER );
var
  R: TRect;
  Str: string;
  CaptionHeight_: integer;
  x,x_,y,y_,IHeight,IWidth,Index: integer;
begin

  R := ARect;
  if (ARow > 0)and(ACol>0) then begin inherited; exit; end;

    if IsItAFilledBitmap(bmp) then
    begin
      x := r.Left; y := r.top; IHeight := r.bottom-r.top; IWidth := r.Right-r.Left;
      x_:=x; y_:=y;
	while x_ < r.right do
	begin
	  if x_+IWidth > r.right then IWidth:=r.right-x_;
	  while y_ < r.bottom do begin
            IHeight := r.bottom-r.top;
	    if y_+IHeight > r.bottom then IHeight:=r.bottom-y_;
	    BitBlt(Canvas.Handle, x_, y_, min( IWidth, bmp.Width ), min( IHeight, bmp.Height ), bmp.Canvas.Handle, 0,0, SRCCOPY );
	    Inc(y_, min( IHeight, bmp.Height ));
	  end;
	  Inc(x_, min(IWidth, bmp.Width )); y_:=y;
	end;

      if ACol=0 then
      begin
        DrawBoxEx( Canvas.Handle, ARect, ALLGLSIDES, bvNone, bvRaised, false, 0, true );
        exit;
      end;
    end;

    if (ARow <> 0)or(ACol<1) then begin inherited; exit; end;

    Str := Columns[ACol-1].Title.Caption;

    Canvas.Font := Columns[ACol-1].Title.Font;
    InflateRect(ARect, -1, -1 );
    R := ARect;
    InflateRect(R,1,1);
    DrawBoxEx( Canvas.Handle, R, ALLGLSIDES, bvNone, bvRaised, false, Columns[ACol-1].Title.Color, IsItAFilledBitmap(bmp)or (Columns[ACol-1].Title.Color=Color) );

    if Assigned(FGlyphs) then
    begin
      if FSingleGlyph then Index := 0 else Index := ACol-1;
      if Index < FGlyphs.Count then
      begin
        FGlyphs.GetBitmap(Index,Glyph);
        CreateBitmapExt( Canvas.Handle, Glyph,
    		         R, 2, max( 0, (R.Bottom-R.Top-Glyph.Height)div 2),
                         fwoNone, fdsDefault, true,
                         GetTransparentColor(Glyph, ftcLeftBottomPixel), 0
                        );
        inc( ARect.Left, Glyph.Width ); R := ARect;
      end;
    end;

    SetBkMode( Canvas.Handle, TRANSPARENT );
    DrawText( Canvas.Handle, PChar(Str), -1, R, aAlignments[FAlignment] or DT_WORDBREAK or
              DT_CALCRECT );

    if FCaptionHeight < 0 then CaptionHeight_ := R.Bottom - R.top
                          else CaptionHeight_ := FCaptionHeight;

    RowHeights[0] := CaptionHeight_;

    ARect.Top := ARect.Top + max( 0, (ARect.Bottom-R.Bottom) div 2 );
    DrawText( Canvas.Handle, PChar(Str), -1, ARect, aAlignments[FAlignment] or DT_WORDBREAK );
//              DT_CENTER or DT_WORDBREAK );

end;

procedure TglDBGrid.GlyphsListChanged(Sender: TObject);
begin
  Repaint;
end;

//*****************************************_____________PROPERTY METHODS

procedure TglDBGrid.SetAlignment(Value: TAlignment);
begin FAlignment := Value; Repaint; end;

procedure TglDBGrid.SetCaptionHeight(Value: integer);
begin FCaptionHeight := Value; if FCaptionHeight>=0 then RowHeights[0]:=FCaptionHeight else Repaint; end;

function  TglDBGrid.GetBitmap: TBitmap;
begin
  if not Assigned(FBitmap) then FBitmap := TBitmap.Create;
  Result := FBitmap;
end;

procedure TglDBGrid.SetBitmap(Value: TBitmap);
begin
  if Assigned(FBitmap) then FBitmap.Free;
  FBitmap:=TBitmap.Create;
  FBitmap.Assign(Value);
  if Assigned(Value) then Bmp := FBitmap else
    if Assigned(FImage)and Assigned(FImage.Picture)and Assigned(FImage.Picture.Bitmap) then
      Bmp := FImage.Picture.Bitmap else Bmp := nil;
  Invalidate;
end;

procedure TglDBGrid.SetImage(Value: TImage);
begin
  FImage := Value;
  if Assigned(FImage)and Assigned(FImage.Picture)and Assigned(FImage.Picture.Bitmap) then
    Bmp := FImage.Picture.Bitmap else if Assigned(FBitmap) then Bmp := FBitmap
                                                           else Bmp := nil;
  Invalidate;
end;


procedure TglDBGrid.SetGlyphs(Value: TImageList);
begin
  if Assigned(FGlyphs) then  FGlyphs.UnregisterChanges(GlyphsChangeLink);
  FGlyphs := Value;
  if Assigned(FGlyphs) then
    FGlyphs.RegisterChanges(GlyphsChangeLink);

end;

procedure TglDBGrid.SetSingleGlyph(Value: boolean);
begin
  FSingleGlyph:=Value;  Repaint;
end;
//-------------------------------------------------------------------------------


end.


