{ 
  								  
 		 Globus Delphi VCL Extensions Library		   
 			  ' GLOBUS LIB '			   
  			     Freeware				  
  	  Copyright (c) 1998 Chudin A.V, FidoNet: 1246.16	  
  								  
  
 ===================================================================
 glBitBtn Unit 05.2000				 component TglBitBtn

 ===================================================================
}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unit glBitBtn;

interface
{$I glDEF.INC}
uses
  Windows, Messages, Classes, Controls, Graphics, glTypes, glCommCl,
  glUtils, ExtCtrls, buttons;
type

  TglBitBtn = class(TBitBtn)
  private
    FCanvas		: TCanvas;
    fMouseEnter         : boolean;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
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
  RegisterComponents('Proba', [TglBitBtn]);
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _
constructor TglBitBtn.Create( AOwner : TComponent );
begin
  inherited;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;//...i can draw now! :)
  //..defaults
end;

destructor TglBitBtn.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

procedure TglBitBtn.CNDrawItem(var Message: TWMDrawItem);
begin
  inherited;
  DrawItem(Message.DrawItemStruct^);
end;

procedure TglBitBtn.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
  IsDown{, IsDefault}: Boolean;
  //State: TButtonState;
  R: TRect;
  BPen, FPen, SPen,OldPen :HPEN;
  FBrush: HBRUSH;
begin
  R := ClientRect;

  with DrawItemStruct do
  begin
    IsDown := itemState and ODS_SELECTED <> 0;
    //IsDefault := itemState and ODS_FOCUS <> 0;

    {if not Enabled then State := bsDisabled
    else if IsDown then State := bsDown
    else State := bsUp;}
  end;
  R := ClientRect;
  if (not fMouseEnter) and (not IsDown) then
  begin

    FBrush := CreateSolidBrush( GetSysColor(COLOR_BTNFACE) );
    if not Focused and not Default then
    begin
      SPen := CreatePen( PS_SOLID, 1, GetSysColor(COLOR_BTNSHADOW) );
      FPen := CreatePen( PS_SOLID, 1, GetSysColor(COLOR_BTNFACE) );
      BPen := CreatePen( PS_SOLID, 1, ColorToRGB((Parent as TWinControl).Brush.Color) );
      OldPen := SelectObject( DrawItemStruct.hDC, FPen );

      MoveToEx(DrawItemStruct.hDC, R.Left+1, R.Top+1, nil); LineTo(DrawItemStruct.hDC, R.Right-1, R.Top+1);
      MoveToEx(DrawItemStruct.hDC, R.Left+1, R.Top+1, nil); LineTo(DrawItemStruct.hDC, R.Left+1, R.Bottom-1);

      SelectObject( DrawItemStruct.hDC, BPen );

      MoveToEx(DrawItemStruct.hDC, R.Left, R.Bottom-1, nil); LineTo(DrawItemStruct.hDC, R.Right, R.Bottom-1);
      MoveToEx(DrawItemStruct.hDC, R.Right-1, R.Top, nil); LineTo(DrawItemStruct.hDC, R.Right-1, R.Bottom);

      SelectObject( DrawItemStruct.hDC, SPen );

      MoveToEx(DrawItemStruct.hDC, R.Left-2, R.Bottom-2, nil); LineTo(DrawItemStruct.hDC, R.Right-1, R.Bottom-2);
      MoveToEx(DrawItemStruct.hDC, R.Right-2, R.Top, nil); LineTo(DrawItemStruct.hDC, R.Right-2, R.Bottom-1);

      DeleteObject( SelectObject( DrawItemStruct.hDC, OldPen ) );
      DeleteObject(FPen);
      DeleteObject(BPen);
    end else
    begin
      FrameRect(DrawItemStruct.hDC, Rect(R.Left+2, R.Top+2, R.Right-2, R.Bottom-2), FBrush);
      DeleteObject(FBrush);
    end;
  end;

end;

procedure TglBitBtn.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  fMouseEnter := true;
  Repaint;
end;

procedure TglBitBtn.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  fMouseEnter := false;
  Repaint;
end;



end.
