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
unit glBevel;

interface
{$I glDEF.INC}
uses
  Windows, Messages, Classes, Controls, Graphics, glTypes, glCommCl,
  glUtils, ExtCtrls;
type

  TglBevel = class(TGraphicControl)
  private
    FBevelInner 	: TPanelBevel;
    FBevelOuter 	: TPanelBevel;
    FBevelSides 	: TglSides;
    FBevelBold		: boolean;
    FBevelPenStyle	: TPenStyle;
    FBevelPenWidth	: word;
    FInteriorOffset	: word;
    FGradient		: TGradient;
    FVertLines          : TglBevelLines;
    FHorLines           : TglBevelLines;
//    FMouseSentencive	  : boolean;
    FExternalCanvas     : TCanvas;
    procedure OnSmthChanged(Sender: TObject);

    procedure SetBevelInner(Value: TPanelBevel);
    procedure SetBevelOuter(Value: TPanelBevel);
    procedure SetBevelSides(Value: TglSides);
    procedure SetBevelBold(Value: boolean);
    procedure SetBevelPenStyle(Value: TPenStyle);
    procedure SetBevelPenWidth(Value: word);
    procedure SetInteriorOffset(Value: word);

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
  public
    Ctrl3D: boolean;
    procedure Paint; override;
    property ExternalCanvas: TCanvas read FExternalCanvas write FExternalCanvas;
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
    procedure Loaded; override;

  published
  {$IFDEF GLVER_D5}
    property Anchors;
  {$ENDIF}  
    property Align;
    property ParentShowHint;
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
    property OnStartDrag;
    property DragCursor;
    property DragMode;
    
    property Canvas;
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner
      default bvLowered;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter
      default bvNone;
    property BevelSides: TglSides read FBevelSides write SetBevelSides
      default [ fsdLeft, fsdTop, fsdRight, fsdBottom ];
    property BevelBold: boolean read FBevelBold write SetBevelBold
      default false;
    property BevelPenStyle: TPenStyle read FBevelPenStyle write SetBevelPenStyle
      default psSolid;
    property BevelPenWidth: word read FBevelPenWidth write SetBevelPenWidth
      default 1;
    property InteriorOffset: word read FInteriorOffset write SetInteriorOffset
      default 0;
    property Gradient :TGradient read FGradient write FGradient;
    property VertLines: TglBevelLines read FVertLines write FVertLines;
    property HorLines: TglBevelLines read FHorLines write FHorLines;
  end;

procedure Register;

implementation
{~~~~~~~~~~~~~~~~~~~~~~~~~}
procedure Register;
begin
  RegisterComponents('GL Controls', [TglBevel]);
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _
constructor TglBevel.Create( AOwner : TComponent );
begin
  inherited;
  FGradient  := TGradient.Create;
  FVertLines := TglBevelLines.Create;
  FHorLines  := TglBevelLines.Create;
  //..defaults
  Width:=50; Height:=50;
  FBevelInner := bvLowered;
//  FBevelOuter := bvNone;
  FBevelSides := [ fsdLeft, fsdTop, fsdRight, fsdBottom ];
  FBevelPenStyle := psSolid;
  FBevelPenWidth := 1;
  Ctrl3D := true;
  FGradient.OnChanged := OnSmthChanged;
  FVertLines.OnChanged := OnSmthChanged;
  FHorLines.OnChanged  := OnSmthChanged;
end;

destructor TglBevel.Destroy;
begin
  Gradient.Free;
  FVertLines.Free;
  FHorLines.Free;
  inherited;
end;

procedure TglBevel.Paint;
var
  r, r_: TRect;
  BoxSides: TglSides;
  TargetCanvas: TCanvas;
  procedure DrawLines( r_: TRect; Direction: TglLinesDir; Lines: TglBevelLines );
  var i: integer;
  begin
    if Direction = fldVertical then
    begin
      if Ctrl3D then BoxSides := [ fsdLeft, fsdRight ] else BoxSides := [ fsdLeft ];
      if Lines.IgnoreBorder then begin r_.top := r.top; r_.bottom := r.bottom; end;
    end else
    begin
      if Ctrl3D then BoxSides := [ fsdTop, fsdBottom ] else BoxSides := [ fsdTop ];
      if Lines.IgnoreBorder then begin r_.left := r.left; r_.right := r.right; end;
    end;

    for i:=1 to Lines.Count do
    begin
      case Direction of
        fldVertical:
        begin
          r_.left := MulDiv( i, Width, Lines.Count+1);
	  r_.Right := r_.Left + Lines.Thickness + integer(Lines.Bold);
        end;
        else{fldHorizontal:}
        begin
          r_.Top := MulDiv( i, Height, Lines.Count+1);
//	  if i = 1 then dec( r_.Top, Lines.Thickness );
	  r_.Bottom := r_.Top + Lines.Thickness + integer(Lines.Bold);
        end;
      end;
      {$IFDEF GLVER_D5}
      if Lines.Style = bvSpace then
        BoxSides := [ fsdLeft, fsdTop];
      {$ENDIF}

      DrawBoxEx( TargetCanvas.Handle, r_, BoxSides, Lines.Style, bvNone,
	         Lines.Bold, 0, true );
    end;
  end;
begin
  if Assigned(ExternalCanvas) then TargetCanvas := ExternalCanvas
                              else TargetCanvas := Canvas;
  r := ClientRect;
  InflateRect( r, -FInteriorOffset, -FInteriorOffset );
  GradientBox( TargetCanvas.handle, r, Gradient,
	       integer(FBevelPenStyle), FBevelPenWidth );

  r := ClientRect; dec( r.right ); dec( r.bottom );
  TargetCanvas.Pen.Width:=FBevelPenWidth; TargetCanvas.Pen.Style:=FBevelPenStyle;
  r_ := DrawBoxEx( TargetCanvas.Handle, r, BevelSides, BevelInner, BevelOuter,
		   FBevelBold, 0, true );

  DrawLines( r_, fldHorizontal, HorLines );
  DrawLines( r_, fldVertical, VertLines );
end;

procedure TglBevel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TglBevel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
end;

procedure TglBevel.OnSmthChanged(Sender: TObject);
begin Repaint; end;
//...______________________________________________PROPERTIES METHODS
procedure TglBevel.SetBevelOuter(Value: TPanelBevel);
//var r: TRect;
begin
  if FBevelOuter=Value then exit;
//  r:=ClientRect; InflateRect( r, -5, -5 );
  FBevelOuter:=Value;
  Invalidate;// ValidateRect( canvas.handle, @r );
end;

procedure TglBevel.SetBevelInner(Value: TPanelBevel);
//var r: TRect;
begin
  if FBevelInner=Value then exit;
//  r:=ClientRect; InflateRect( r, -5, -5 );
  FBevelInner:=Value;
  Invalidate; //ValidateRect( canvas.handle, @r );
end;

procedure TglBevel.SetBevelSides(Value: TglSides);
begin
  if FBevelSides=Value then exit;
  FBevelSides:=Value; Invalidate;
end;

procedure TglBevel.SetBevelBold(Value: boolean);
begin
  if FBevelBold=Value then exit;
  FBevelBold:=Value; Invalidate;
end;

procedure TglBevel.SetBevelPenStyle(Value: TPenStyle);
begin
  if FBevelPenStyle=Value then exit;
  FBevelPenStyle:=Value; Invalidate;
end;

procedure TglBevel.SetBevelPenWidth(Value: word);
begin
  if FBevelPenWidth=Value then exit;
  FBevelPenWidth:=Value; Invalidate;
end;

procedure TglBevel.SetInteriorOffset(Value: word);
begin
  if FInteriorOffset=Value then exit;
  FInteriorOffset:=Value; Invalidate;
end;

procedure TglBevel.Loaded;
begin
  inherited;
  if FGradient.Active then
    ControlStyle := ControlStyle + [csOpaque];
end;

end.
