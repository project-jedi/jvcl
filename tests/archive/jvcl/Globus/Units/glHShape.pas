{ 
  								  
 		 Globus Delphi VCL Extensions Library		   
 			  ' GLOBUS LIB '			   
  			     Freeware				  
  	  Copyright (c) 1998 Chudin A.V, FidoNet: 1246.16	  
  								  
  
 ===================================================================
 glShape Unit xx.1998				  component TglShape
 ===================================================================}
unit glHShape;

//...last modified: 30.09.2002 - ShapeBitmap fixed 

interface
{$I glDEF.INC}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, glTypes, glUtils, glCommCl;

type
  TRGNCombineMode = ( cmAND, cmCOPY, cmDIFF, cmOR, cmXOR );
  THoleShapeType = (stRectangle, stSquare, stRoundRect, stRoundSquare, stEllipse, stCircle);
  TglHoleShape = class(TGraphicControl)
  private
    FShape		: THoleShapeType;
    FShapeBitmap	: TBitmap;
    FBevelInner 	: TPanelBevel;
    FBevelOuter 	: TPanelBevel;
    FBoldInner		: boolean;
    FBoldOuter		: boolean;
    FRectEllipse	: TPointClass;
    FBevelOffset	: integer;
    fNeedUpdateRGN	: boolean;
    fDestroyed		: boolean;
    fRunOnce		: boolean;
    fNeedRebuildBitmapShape : boolean;
    OldX,OldY,OldW,OldH : integer;
    procedure SetEnabled( Value: boolean );
    procedure SetEnabledDT( Value: boolean );
    procedure SetShape( Value: THoleShapeType );
    procedure SetShapeBitmap( Value: TBitmap );
    procedure SetBevelInner( Value: TPanelBevel );
    procedure SetBevelOuter( Value: TPanelBevel );
    procedure SetBoldInner( Value: boolean );
    procedure SetBoldOuter( Value: boolean );
    procedure SetCombineMode( Value: TRGNCombineMode );
    procedure SetBevelOffset( Value: integer );

    procedure Update_;
    procedure CalcRGNs;
    procedure SmthChanged(Sender: TObject);
    procedure SayAllDTEnabledState( EnabledDT: boolean );
  protected
    procedure Paint; override;
  public
    RGNOuter, RGNInner	: HRGN;
    FCombineMode	: TRGNCombineMode;
    FEnabledDT		: boolean;
    FEnabled		: boolean;
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
    procedure UpdateRGN;
    procedure Loaded; override;
  published
    property Align;
    property ShowHint;
    property ParentShowHint;
    property PopupMenu;
//    property Visible;
    property Enabled: boolean read FEnabled write SetEnabled
     default true;
    property EnabledAllInDesignTime: boolean read FEnabledDT write SetEnabledDT
     default true;
    property Shape: THoleShapeType read FShape write SetShape
     default stEllipse;
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner
     default bvNone;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter
     default bvLowered;
    property BevelInnerBold: boolean read FBoldInner write SetBoldInner
     default true;
    property BevelOuterBold: boolean read FBoldOuter write SetBoldOuter
     default true;
    property CombineMode: TRGNCombineMode read FCombineMode write SetCombineMode
     default cmDIFF;
    property BevelOffset: integer read FBevelOffset write SetBevelOffset
     default 0;
    property RectEllipse: TPointClass read FRectEllipse write FRectEllipse;
    property ShapeBitmap: TBitmap read FShapeBitmap write SetShapeBitmap;
  end;

procedure Register;

implementation
const
  aCombMode : array[0..4] of integer = (RGN_AND, RGN_COPY, RGN_DIFF, RGN_OR, RGN_XOR );
{~~~~~~~~~~~~~~~~~~~~~~~~~}
procedure Register;
begin
  RegisterComponents('Proba', [TglHoleShape]);
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _
constructor TglHoleShape.Create( AOwner : TComponent );
begin
  inherited;
  FShapeBitmap:=TBitmap.Create;
  FEnabled := (Owner is TWinControl);

  ControlStyle := ControlStyle - [csOpaque];
  FEnabledDT:=FEnabled;
  fDestroyed:=false;
  FRectEllipse:=TPointClass.Create;
  FRectEllipse.x:=30; FRectEllipse.y:=30;
  FRectEllipse.OnChanged:=SmthChanged;
  FShape:=stEllipse;
  FBevelOuter:=bvLowered;
  FBevelInner:=bvNone;
  FCombineMode:=cmDIFF;
  FBoldInner:=true; FBoldOuter:=true;
  FRectEllipse.y:=45; FRectEllipse.x:=45;
  FBevelOffset:=0;
  Width:=112; Height:=112;
  fNeedUpdateRGN:=false;
  fRunOnce:=true;
end;

destructor TglHoleShape.Destroy;
begin
  FShapeBitmap.Free;
  FRectEllipse.Free;
  if not (csDestroying in Owner.ComponentState) then
  begin FEnabledDT:=false; FEnabled:=false; UpdateRGN(); end;
  inherited;
end;

procedure TglHoleShape.Paint;
var
  r		  :TRect;
  H,W,EH,EW,i	  :integer;

procedure DrawShape( Bevel: TPanelBevel; fBold, fRect: boolean );//_______LOCAL PROC_
  procedure SetPenAndBrush( c: Tcolor );
  begin
    Canvas.Pen.Color:=c;
    if fRect and ((EW and EH)=0) then
    begin Canvas.Brush.Style:=bsClear; end
    else begin Canvas.Brush.Color:=c; end
  end;
begin
  Canvas.Brush.Style:=bsClear;//bsSolid;//bsClear;
  i := integer( fBold );
  with Canvas do
  case Bevel of
    bvLowered:
    begin
      SetPenAndBrush( clBtnHighlight );
      if fRect then RoundRect( R.Left, R.Top, R.Right, R.Bottom, EW, EH)
	       else Ellipse( R.Left, R.Top, R.Right, R.Bottom );
      SetPenAndBrush( clBtnShadow );
      if fRect then RoundRect( R.Left, R.Top, R.Right-1, R.Bottom-1, EW, EH)
	       else Ellipse( R.Left, R.Top, R.Right-1, R.Bottom-1 );
      if FBold then begin
	SetPenAndBrush( cl3DDkShadow );
	if fRect then RoundRect( R.Left+1, R.Top+1, R.Right-1, R.Bottom-1, EW, EH)
		 else Ellipse( R.Left+1, R.Top+1, R.Right-1, R.Bottom-1 );
      end;
      InflateRect( R, -1, -1 ); inc(R.Left,i); inc(R.Top,i);
    end;
    bvRaised:
    begin
      SetPenAndBrush( clBtnHighlight );
      if fRect then RoundRect( R.Left, R.Top, R.Right, R.Bottom, EW, EH)
	       else Ellipse( R.Left, R.Top, R.Right, R.Bottom );
      if FBold then begin
	SetPenAndBrush( cl3DDkShadow );
	if fRect then RoundRect( R.Left+1, R.Top+1, R.Right, R.Bottom, EW, EH)
		 else Ellipse( R.Left+1, R.Top+1, R.Right, R.Bottom );
      end;
      SetPenAndBrush( clBtnShadow );
      if fRect then RoundRect( R.Left+1, R.Top+1, R.Right-i, R.Bottom-i, EW, EH)
	       else Ellipse( R.Left+1, R.Top+1, R.Right-i, R.Bottom-i );
      InflateRect( R, -1, -1 ); dec(R.Right,i); dec(R.Bottom,i);
    end;
    else
    begin
      //Brush.Color:=clBlack;
      //FrameRect( Rect(Left, Top, Left+W, Top+H) );
    end;
  end;
    SetPenAndBrush( clBtnFace );

end;//____________________________________END LOCAL PROC_

begin//_________________________________________________________PAINT_
  fNeedUpdateRGN := fNeedUpdateRGN or(OldX<>Left)or(OldY<>Top)or(OldW<>Width)or(OldH<>Height);

  if fNeedUpdateRGN then UpdateRGN();
  OldX:=Left; OldY:=Top; OldW:=Width; OldH:=Height;

  if IsItAFilledBitmap( FShapeBitmap ) then
  begin
    BitBlt( Canvas.handle, -1,-1, Width, Height, FShapeBitmap.Canvas.handle, 0,0, SRCCopy);
    exit;
  end;

  case FShape of
    stRectangle, stRoundRect, stEllipse:
      begin H:=Height; W:=Width; end
    else
      begin H:=min(Height,Width); W:=H; end;
  end;
  R := Bounds( 0, 0, W, H );
  with Canvas do
  case FShape of
    stRectangle, stSquare, stRoundRect, stRoundSquare:
      begin
	if (FShape = stRectangle)or(FShape = stSquare) then
	begin EW:=0; EH:=0; end;
	if (FShape = stRoundRect)or(FShape = stRoundSquare) then
	begin EW:=FRectEllipse.x; EH:=FRectEllipse.y; end;

	DrawShape( FBevelOuter, FBoldOuter, true );
	InflateRect( R, -FBevelOffset, -FBevelOffset );
	DrawShape(FBevelInner, FBoldInner, true );

	//Pen.Color:=clBtnFace;
	//Rect( R.Left, R.Top, R.Right, R.Bottom );
      end;
    stEllipse, stCircle:
      begin
	DrawShape( FBevelOuter, FBoldOuter, false );
	InflateRect( R, -FBevelOffset, -FBevelOffset );
	DrawShape(FBevelInner, FBoldInner, false );
      end;
  end;
end;
//-------------------------------------------------------
procedure TglHoleShape.CalcRGNs;
var
  H, W, xOffs, yOffs	:integer;
  R			:TRect;
  BmpInfo		:Windows.TBitmap;
  BorderStyle: TFormBorderStyle;
  procedure CalcShape( Bevel: TPanelBevel; fBold: boolean );//____LOCAL PROC_
  var
    i: integer;
  begin
    i := integer( fBold );
    case Bevel of
      bvLowered: begin InflateRect( R, -1, -1 ); inc(R.Left,i); inc(R.Top,i); end;
      bvRaised: begin InflateRect( R, -1, -1 ); dec(R.Right,i); dec(R.Bottom,i); end;
    end;
  end;//____________________________________END LOCAL PROC_
  procedure CalcBmpRgn(var rgn: HRGN );
  var
    i,j	:integer;
    rgn2: HRGN;
    TransparentColor: TColor;
  begin
    TransparentColor := FShapeBitmap.Canvas.Pixels[0, FShapeBitmap.Height-1];
    for j:=0 to FShapeBitmap.Height do
    for i:=0 to FShapeBitmap.Width do
    begin
      if FShapeBitmap.Canvas.Pixels[i,j] <> TransparentColor then continue;
      RGN2 := CreateRectRgn(i, j, i+1, j+1);
      CombineRgn( RGN, RGN2, RGN, RGN_OR );
      DeleteObject( RGN2 );
    end;
  end;//____________________________________END LOCAL PROC_
begin
  if not FShapeBitmap.Empty then
  begin
    {if fNeedRebuildBitmapShape then} with FShapeBitmap do
    begin
      GetObject( FShapeBitmap.Handle, sizeof(Windows.TBitmap), @BmpInfo );
      DeleteObject( RGNOuter );
      DeleteObject( RGNInner );
      RGNInner := CreateRectRgn(0, 0, 0, 0);
      CalcBmpRgn(RGNInner);
      fNeedRebuildBitmapShape := false;
    end;
  end
  else
  begin
    case FShape of
      stRectangle, stRoundRect, stEllipse:
        begin H:=Height; W:=Width; end
      else
        begin H:=min(Height,Width); W:=H; end;
    end;
    R := Bounds( 0, 0, W, H );
    DeleteObject( RGNOuter );
    DeleteObject( RGNInner );

    if FBevelOffset <> 0 then
    begin
      CalcShape( FBevelOuter, FBoldOuter );
      OffsetRect(R,1,1);
    end;
    case FShape of
      stRectangle, stSquare:
        RGNOuter := CreateRectRgn( R.Left, R.Top, R.Right, R.Bottom );
      stRoundRect, stRoundSquare:
        RGNOuter := CreateRoundRectRgn( R.Left, R.Top, R.Right, R.Bottom, FRectEllipse.x, FRectEllipse.y );
      stEllipse, stCircle:
        RGNOuter := CreateEllipticRgn( R.Left, R.Top, R.Right, R.Bottom );
    end;
    if FBevelOffset=0 then CalcShape( FBevelOuter, FBoldOuter );
    InflateRect( R, -FBevelOffset, -FBevelOffset );
    if FBevelOffset=0 then CalcShape( FBevelInner, FBoldInner )
		    else OffsetRect(R,-1,-1);
    case FShape of
      stRectangle, stSquare:
        RGNInner := CreateRectRgn( R.Left+1, R.Top+1, R.Right+1, R.Bottom+1 );
      stRoundRect, stRoundSquare:
        RGNInner := CreateRoundRectRgn( R.Left+1, R.Top+1, R.Right+2, R.Bottom+2, FRectEllipse.x, FRectEllipse.y );
      stEllipse, stCircle:
        RGNInner := CreateEllipticRgn( R.Left+1, R.Top+1, R.Right+2, R.Bottom+2 );
    end;
  end;

    { calc offsets }
    if Owner is TForm then
    begin
      if csDesigning in ComponentState then BorderStyle := bsSizeable
				       else BorderStyle := TForm(Owner).BorderStyle;
      case BorderStyle of
        bsSizeable:
        begin
  	  xOffs := GetSystemMetrics(SM_CXFRAME)-1;
	  yOffs := GetSystemMetrics(SM_CYFRAME)-1;
	  inc( yOffs, GetSystemMetrics(SM_CYCAPTION) );
        end;
        bsDialog:
        begin
	  xOffs := GetSystemMetrics(SM_CXDLGFRAME)-1;
	  yOffs := GetSystemMetrics(SM_CYDLGFRAME)-1;
	  inc( yOffs, GetSystemMetrics(SM_CYCAPTION) );
        end;
        bsSingle:
        begin
	  xOffs := GetSystemMetrics(SM_CXBORDER);
	  yOffs := GetSystemMetrics(SM_CYBORDER);
	  inc( yOffs, GetSystemMetrics(SM_CYCAPTION) );
        end;
        bsToolWindow:
        begin
	  xOffs := GetSystemMetrics(SM_CXBORDER);
	  yOffs := GetSystemMetrics(SM_CYBORDER);
	  inc( yOffs, GetSystemMetrics(SM_CYSMCAPTION) );
        end;
        bsSizeToolWin:
        begin
	  xOffs := GetSystemMetrics(SM_CXSIZEFRAME);
	  yOffs := GetSystemMetrics(SM_CYSIZEFRAME);
	  inc( yOffs, GetSystemMetrics(SM_CYSMCAPTION) );
        end;
        else
        begin
          xOffs := -1;
          yOffs := -1;
        end;
      end;

      OffsetRgn( RGNInner, Left+xOffs, Top+yOffs );
      OffsetRgn( RGNOuter, Left+xOffs, Top+yOffs );
    end;

  fRunOnce:=false;
end;
//-------------------------------------------------------
//...set all enabled/disabled in design time
procedure TglHoleShape.SayAllDTEnabledState( EnabledDT: boolean );
var
  i: integer;
begin
  for i:=0 to TWinControl(Owner).ControlCount-1 do with TWinControl(Owner) do
  begin
    if (Controls[i] is TglHoleShape) then
    begin
      TglHoleShape(Controls[i]).FEnabledDT := EnabledDT;
    end;
  end;

end;
//-------------------------------------------------------
procedure TglHoleShape.UpdateRGN;
var
  i: integer;
  NewRGN: HRGN;
begin
  if not(Owner is TWinControl) then exit;
  NewRGN := CreateRectRgn( 0, 0, 2000, 1000 );

  for i:=0 to TWinControl(Owner).ControlCount-1 do with TWinControl(Owner) do
  begin
    if Controls[i] is TglHoleShape then
      with TglHoleShape(Controls[i])do
        if ((csDesigning in ComponentState)and FEnabledDT)
           or ((not(csDesigning in ComponentState))and FEnabled) then
        begin
          CalcRGNs;
          CombineRgn( NewRGN, NewRGN, RGNInner, aCombMode[ integer(FCombineMode) ] )
        end;
      end;

  SetWindowRgn( TWinControl(Owner).Handle, NewRGN, true );
  fNeedUpdateRGN:=false;
end;

procedure TglHoleShape.Update_;
begin
  if csLoading in ComponentState then exit;
  UpdateRGN();
  Refresh;
end;

procedure TglHoleShape.SmthChanged(Sender: TObject);
begin
  Update_;
end;

//________________________________________________________ Properties _
procedure TglHoleShape.SetEnabled( Value: boolean );
begin
  if (FEnabled = Value)or not(Owner is TWinControl) then exit;
  FEnabled := Value; Update_;
end;

procedure TglHoleShape.SetEnabledDT( Value: boolean );
begin
  if (FEnabledDT = Value)or not(Owner is TWinControl) then exit;
  FEnabledDT := Value; SayAllDTEnabledState( FEnabledDT );
  Update_;
end;

procedure TglHoleShape.SetShape( Value: THoleShapeType );
begin
  if FShape = Value then exit;
  FShape := Value; Update_;
end;

procedure TglHoleShape.SetShapeBitmap( Value: TBitmap );
begin
  if FShapeBitmap = Value then exit;
  fNeedRebuildBitmapShape := true;
  FShapeBitmap.Assign(Value);
  if Assigned(FShapeBitmap) then
  begin
    Width := FShapeBitmap.Width;
    Height := FShapeBitmap.Width;
  end;
  Update_();
end;

procedure TglHoleShape.SetBevelInner( Value: TPanelBevel );
begin
  if FBevelInner = Value then exit;
  FBevelInner := Value; Update_;
end;

procedure TglHoleShape.SetBevelOuter( Value: TPanelBevel );
begin
  if FBevelOuter = Value then exit;
  FBevelOuter := Value; Update_;
end;

procedure TglHoleShape.SetBoldInner( Value: boolean );
begin
  if FBoldInner = Value then exit;
  FBoldInner := Value; Update_;
end;

procedure TglHoleShape.SetBoldOuter( Value: boolean );
begin
  if FBoldOuter = Value then exit;
  FBoldOuter := Value; Update_;
end;

procedure TglHoleShape.SetCombineMode( Value: TRGNCombineMode );
begin
  if FCombineMode = Value then exit;
  FCombineMode := Value; Update_;
end;

procedure TglHoleShape.SetBevelOffset( Value: integer );
begin
  if (FBevelOffset = Value)or(Value < 0) then exit;
  if (Value > width-2)or(Value > height-2) then Value:=min(width,height)-2;
  FBevelOffset := Value; Update_;
end;

procedure TglHoleShape.Loaded;
begin
  inherited;
  fNeedRebuildBitmapShape := true;
  UpdateRGN(); Refresh;
end;

end.
