{ 
  								  
 		 Globus Delphi VCL Extensions Library		   
 			  ' GLOBUS LIB '			   
  			     Freeware				  
  	  Copyright (c) 1998 Chudin A.V, FidoNet: 1246.16	  
  								  
  
 ===================================================================
 glGraphicButton Unit 11.1999 		   component glGraphicButton
 ===================================================================
}
unit glGBtn;
{$I glDEF.INC}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,controls, glTypes, glUtils;

type

  TGButtonState = ( bsActive, bsPassive, bsPushed);

  TglGraphicButton = class(TGraphicControl)
  private
    FAutoSize		: boolean;
    FGlyphActive	: TBitmap;
    FGlyphPassive	: TBitmap;
    FGlyphPushed	: TBitmap;
    State               : TGButtonState;
    FOnMouseEnter       : TNotifyEvent;
    FOnMouseLeave       : TNotifyEvent;
    procedure SetGlyphActive(Value: TBitmap);
    procedure SetGlyphPassive(Value: TBitmap);
    procedure SetGlyphPushed(Value: TBitmap);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
//    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;


  public
    procedure Paint; override;
    property Canvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property GlyphActive: TBitmap read FGlyphActive write SetGlyphActive;
    property GlyphPassive: TBitmap read FGlyphPassive write SetGlyphPassive;
    property GlyphPushed: TBitmap read FGlyphPushed write SetGlyphPushed;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Proba', [TglGraphicButton]);
end;

//*****************************************_____________LowLevel METHODS
//________________________________________________________
constructor TglGraphicButton.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);
//  ControlStyle := ControlStyle + [{csReplicatable,}csOpaque];
  {inherited }Width := 105;
  {inherited }Height := 105;

  FGlyphActive:=TBitmap.create;
  FGlyphPassive:=TBitmap.create;
  FGlyphPushed:=TBitmap.create;
  //...defaults
  FAutoSize	      := false;
  State := bsPassive;
end;
//________________________________________________________
destructor TglGraphicButton.Destroy;
begin
  FGlyphActive.Free;
  FGlyphPassive.Free;
  FGlyphPushed.Free;
  inherited;
end;
//________________________________________________________
procedure TglGraphicButton.Paint;
var
    Glyph: TBitmap;
begin
  case State of
    bsActive:  if Assigned(FGlyphActive) then Glyph := FGlyphActive else Glyph := FGlyphPassive;
    bsPassive: Glyph := FGlyphPassive;
    else{bsPushed}
    begin
      if Assigned(FGlyphPushed) then Glyph := FGlyphPushed else Glyph := FGlyphActive;
      if not Assigned(Glyph) then Glyph := FGlyphPassive;
    end;
  end;
  if Assigned(Glyph) then
    BitBlt( Canvas.Handle, 0, 0, Glyph.Width, Glyph.Height,Glyph.Canvas.Handle, 0, 0, SRCCOPY);
  if (csDesigning in ComponentState)and(tag<>9999) then with Canvas do
  begin
    Pen.Color := clBlack; Pen.Style := psDash; Brush.Style := bsClear;
    Rectangle( 0, 0, width, height );
  end;

end;
//________________________________________________________
procedure TglGraphicButton.SetGlyphActive(Value: TBitmap);
begin
  FGlyphActive.Assign(Value); Repaint;
end;

procedure TglGraphicButton.SetGlyphPassive(Value: TBitmap);
begin
  FGlyphPassive.Assign(Value); Repaint;
end;

procedure TglGraphicButton.SetGlyphPushed(Value: TBitmap);
begin
  FGlyphPushed.Assign(Value); Repaint;
end;


//________________________________________________________
procedure TglGraphicButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  State := bsActive; Paint;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(self);
end;

procedure TglGraphicButton.CMMouseLeave(var Message: TMessage);
begin                                 
  inherited;
  State := bsPassive; Paint;
  if Assigned(FOnMouseLeave) then FOnMouseLeave(self);  
end;

procedure TglGraphicButton.MouseDown(Button: TMouseButton; Shift: TShiftState;	X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button <> mbLeft)or(not Enabled)or(State = bsPassive) then exit;
  State := bsPushed; Paint;
end;

procedure TglGraphicButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (State = bsPushed) and Assigned(OnClick) then OnClick(self);
  if State = bsPushed then State := bsActive else State := bsPassive;
  Paint;
end;


end.
