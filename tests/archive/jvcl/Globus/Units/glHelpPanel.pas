{ 
  								  
 		 Globus Delphi VCL Extensions Library		   
 			  ' GLOBUS LIB '			   
  			     Freeware				  
       Copyright (c) 1998, 2000 Chudin A.V, FidoNet: 1246.16	  
  								  
  
 ===================================================================
 glHelpPanel Unit 12.2000		       omponent TglHelpPanel
 ===================================================================
}
unit glHelpPanel;

interface
{$I glDEF.INC}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, comctrls;

type
  TglHelpPanel = class(TCustomPanel)
  private
    Rich: TRichEdit;
    FStrings: TStrings;
    ButtonRect: TRect;
    FHighlightButton: boolean;
    FExpanded: boolean;
    FExpandedHeight: integer;
    fInitializing: boolean;
    procedure SetStrings(const Value: TStrings);
    procedure SetHighlightButton(const Value: boolean);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetExpanded(const Value: boolean);
    procedure SetExpandedHeight(const Value: integer);

  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure InitRichText;    
    property HighlightButton: boolean read FHighlightButton write SetHighlightButton;
  published
    property Align;
    property Alignment;
  {$IFDEF GLVER_D4}
    property Anchors;
  {$ENDIF}
    property AutoSize;
    property BevelInner stored true;
    property BevelOuter stored true;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
  {$IFDEF GLVER_D4}
    property UseDockManager default True;
    property DockSite;
  {$ENDIF}
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
  {$IFDEF GLVER_D5}
    property OnContextPopup;
  {$ENDIF}
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
  {$IFDEF GLVER_D4}
    property OnEndDock;
  {$ENDIF}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
  {$IFDEF GLVER_D4}
    property OnGetSiteInfo;
  {$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  {$IFDEF GLVER_D4}
    property OnStartDock;
    property OnUnDock;
  {$ENDIF}
    property OnStartDrag;

    property Expanded: boolean read FExpanded write SetExpanded default false;
    property Strings: TStrings read FStrings write SetStrings;
    property ExpandedHeight: integer read FExpandedHeight write SetExpandedHeight;
  end;



procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('Gl Controls', [TglHelpPanel]);
end;

{ TglHelpPanel }

procedure TglHelpPanel.CMMouseLeave(var Message: TMessage);
begin
  HighlightButton := false;
end;

constructor TglHelpPanel.Create(AOwner: TComponent);
begin
  inherited;
  fInitializing := true;

  BevelInner := bvNone;
  BevelOuter := bvNone;

  FStrings := TStringList.Create;
  Height := 70;
  Caption := ' help ';

  //if csDesigning in ComponentState then Align := alBottom;
  Expanded := false;
  fInitializing := false;

  if csDesigning in ComponentState then exit;
  Rich := TRichEdit.Create(self);
  Rich.Parent := self;
  Rich.ReadOnly := true;

end;

destructor TglHelpPanel.Destroy;
begin
  FStrings.Free;
  if Assigned(Rich) then Rich.Free;
  inherited;
end;

procedure TglHelpPanel.Loaded;
begin
  inherited;
  InitRichText;
end;

procedure TglHelpPanel.InitRichText;
var
  ms: TMemoryStream;
begin
  if not Assigned(Rich) then exit;
  Rich.BorderStyle := bsNone;
  Rich.SetBounds(12, 16, Width-24, ExpandedHeight-22);
  ms := TMemoryStream.Create;
  try
    FStrings.SaveToStream(ms);
    ms.Position := 0;
    Rich.Lines.LoadFromStream(ms);
  finally
    ms.Free;
  end;
end;

procedure TglHelpPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if PtInRect(ButtonRect, Point(X, Y)) then
  begin
    Expanded := not Expanded;
    if Assigned(onClick) then onClick(self);
  end;
end;

procedure TglHelpPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if PtInRect(ButtonRect, Point(X, Y)) then
  begin
    if not HighlightButton then HighlightButton := not HighlightButton;
  end else
    if HighlightButton then HighlightButton := not HighlightButton;
end;

procedure TglHelpPanel.Paint;
const
  WARNING = 'Open context menu to load RTF text. Control shows text at runtime only.';
var
  R: TRect;
begin
  //inherited;

  Canvas.Brush.Style := bsSolid;

  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  Canvas.Brush.Color := clBtnShadow;
  Canvas.FillRect(Bounds(5, 7, Width-10, 2));


  Canvas.Brush.Color := clWindow;
  Canvas.Pen.Color := clBlack;
  if Expanded then
    Canvas.Rectangle(5, 15, Width-5, Height-5);

  ButtonRect := Bounds(Width-80, 0, 80, 20);

  Canvas.Font.Style := [fsBold];
  if FHighlightButton then
  begin
    SetBkColor(Canvas.Handle, ColorToRGB(clBtnShadow));
    SetTextColor(Canvas.Handle, clWhite);
  end else
  begin
    SetBkColor(Canvas.Handle, ColorToRGB(clBtnFace));
    SetTextColor(Canvas.Handle, clBlack);
  end;
  SetBkMode(Canvas.Handle, OPAQUE);
  DrawText(Canvas.Handle, PChar(Caption), length(Caption), ButtonRect, DT_SINGLELINE or DT_RIGHT);

  if csDesigning in ComponentState then
  begin
    R := ClientRect; inc(R.Top, 20);
    SetBkMode(Canvas.Handle, TRANSPARENT);
    DrawText(Canvas.Handle, WARNING, length(WARNING), R, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
  end;
end;

procedure TglHelpPanel.SetExpanded(const Value: boolean);
begin
  FExpanded := Value;
  if FExpanded then
    Height := ExpandedHeight
  else
  begin
    FExpandedHeight := Height;
    Height := 16;
  end;

  if not fInitializing then
    if Parent is TForm then with (Parent as TForm) do
      if FExpanded then Height := Height + ExpandedHeight - 16
                   else Height := Height - ExpandedHeight + 16;
end;

procedure TglHelpPanel.SetExpandedHeight(const Value: integer);
begin
  FExpandedHeight := Value;
end;

procedure TglHelpPanel.SetHighlightButton(const Value: boolean);
begin
  FHighlightButton := Value;
  if FHighlightButton then Cursor := crHandPoint else Cursor := crDefault;
  Repaint;
end;

procedure TglHelpPanel.SetStrings(const Value: TStrings);
begin
  FStrings.Assign(Value);
  InitRichText;
end;



end.
