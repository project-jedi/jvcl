{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorForm.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Th鰎nqvist [peter3@peter3.com]
Portions created by Peter Th鰎nqvist are Copyright (C) 2002 Peter Th鰎nqvist.
All Rights Reserved.

Contributor(s):dejoy(dejoy@ynl.gov.cn)

Last Modified: 2004-02-6

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Color form for the @link(TJvColorButton) component

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvOfficeColorForm;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF MSWINDOWS}
  SysUtils, Classes,
{$IFDEF VCL}
  Graphics, Controls, Forms,
{$ENDIF VCL}
{$IFDEF VisualCLX}
  QWindows, Types, Qt, QGraphics, QControls, QStdCtrls, QForms, QExtCtrls,
{$ENDIF VisualCLX}
  JvComponent, JvLabel, JvOfficeColorPanel;

{------------------------------------------------------------------------------}
const
  MinDragBarHeight = 7;
  MinDragBarSpace = 3;

  Tag_DragBarHeight = 9;
  Tag_DragBarSpace = 10;

type

{$IFDEF VCL}
  TJvSubDragBar = class(TJvLabel)
{$ENDIF VCL}
{$IFDEF VisualCLX}
    TJvSubDragBar = class(TLabel)
{$ENDIF VisualCLX}
    private
      OwnerForm: TControl;
{$IFDEF VisualCLX}
      Fx, Fy: integer;
{$ENDIF VisualCLX}
    protected
      procedure MouseEnter(Control: TControl); override;
      procedure MouseLeave(Control: TControl); override;
      procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer); override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer); override;

    end;

  TJvOfficeColorForm = class(TJvForm)
  private

    FBusy: boolean;
    FInited: boolean;
    FWordStyle: Boolean;
    FColorPanel: TJvOfficeColorPanel;
    FDragBar: TJvSubDragBar;

    FOwner: TControl;
    FDragBarSpace: integer;
    FDragBarHeight: integer;
    FFlat: boolean;
    FToolWindowStyle: boolean;
    FOnShowingChanged: TNotifyEvent;
    FOnKillFocus: TNotifyEvent;
    FOnWindowStyleChanged: TNotifyEvent;
    FShowDragBar: Boolean;

    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure SetMeasure(const Index, Value: integer);

    procedure SetFlat(const Value: boolean);
    procedure SetWordStyle(const Value: boolean);
    procedure SetToolWindowStyle(const Value: boolean);

    procedure SetShowDragBar(const Value: Boolean);
  protected

    DropDownMoved: boolean; //移动过
    DropDownMoving: boolean; //正在移动
    MoveEnd: boolean; //移动完毕
    MoveStart: Boolean; //开始移动

    procedure Resize; override;
    procedure Paint; override;

    procedure VisibleChanged; override;

    procedure AdjustColorForm();

{$IFDEF VCL}
    procedure CreateParams(var Params: TCreateParams); override;
{$ENDIF VCL}
{$IFDEF VisualCLX}
    function WidgetFlags: Integer; override;
{$ENDIF VisualCLX}

    procedure DoKillFocus(FocusedWnd: HWND); override;
    procedure ShowingChanged; override;
    property OnShowingChanged: TNotifyEvent read FOnShowingChanged write FOnShowingChanged;
    property OnKillFocus: TNotifyEvent read FOnKillFocus write FOnKillFocus;
    property OnWindowStyleChanged: TNotifyEvent read FOnWindowStyleChanged write FOnWindowStyleChanged;

  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;

    procedure SetButton(Button: TControl);

    property ColorPanel: TJvOfficeColorPanel read FColorPanel;
    property ShowDragBar: Boolean read FShowDragBar write SetShowDragBar default True;
    property DragBarHeight: integer index Tag_DragBarHeight read FDragBarHeight write SetMeasure;
    property DragBarSpace: integer index Tag_DragBarSpace read FDragBarSpace write SetMeasure;
    property ToolWindowStyle: boolean read FToolWindowStyle write SetToolWindowStyle default false;

    property Flat: boolean read FFlat write SetFlat;

  end;

implementation

{TJvOfficeColorForm}

constructor TJvOfficeColorForm.CreateNew(AOwner: TComponent; Dummy: Integer);
var
  ParentControl: TWinControl;
begin
  inherited CreateNew(AOwner, Dummy);
  FInited := False;
  FShowDragBar := True;

  AutoScroll := false;
{$IFDEF VisualCLX}
  BorderIcons := [];
  BorderStyle := fbsDialog;
  Font.Name := 'MS Shell Dlg 2';
{$ELSE}
  BorderStyle := bsDialog;
  BorderWidth := 2;
  AutoSize := True;
{$ENDIF VisualCLX}
  FormStyle := fsStayOnTop;
  Caption := 'Color Window';

  FToolWindowStyle := False;

  ParentControl := Self;

  FDragBar := TJvSubDragBar.Create(self);
  with FDragBar do
  begin
    Parent := ParentControl;
    OwnerForm := self;
    AutoSize := False;
    Caption := '';
{$IFDEF VisualCLX}
    Color := $999999;
{$ELSE}
    Color := clActiveCaption;
{$ENDIF VisualCLX}
    Height := MinDragBarHeight;
  end;

  FColorPanel := TJvOfficeColorPanel.Create(self);
  with FColorPanel do
  begin
    Parent := ParentControl;
  end;

  SetWordStyle(True);
  KeyPreview := True;
  OnDeactivate := FormDeactivate;
  OnKeyUp := FormKeyUp;

  FInited := True;

end;

procedure TJvOfficeColorForm.SetButton(Button: TControl);
begin
  FOwner := Button;
end;

{$IFDEF VCL}

procedure TJvOfficeColorForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not WS_CAPTION;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}

function TJvOfficeColorForm.WidgetFlags: Integer;
begin
  Result := inherited WidgetFlags and
    not Integer(WidgetFlags_WStyle_Title) or
    Integer(WidgetFlags_WType_Popup);
end;

{$ENDIF VisualCLX}

procedure TJvOfficeColorForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Hide;
    ModalResult := mrCancel;
  end;
end;

procedure TJvOfficeColorForm.AdjustColorForm();
var
  TempHeight: integer;
  HasDragBar: boolean;
  Offset: Integer;
begin
  if not FInited or FBusy then exit;
  FBusy := True;

  DisableAlign;

  if ShowDragBar and not ToolWindowStyle then
  begin
    FDragBar.Visible := True;
    HasDragBar := FDragBar.Visible;
    FDragBar.Height := FDragBarHeight;
  end
  else
  begin
    HasDragBar := False;
    FDragBar.Visible := False;
  end;

{$IFDEF VCL}
  Offset := 0;
{$ENDIF VCL}
{$IFDEF VisualCLX}
  Offset := 2;
{$ENDIF VisualCLX}

  if HasDragBar then
    TempHeight := FDragBarHeight + FDragBarSpace * 2
  else
    TempHeight := 0;

  ClientHeight := TempHeight + FColorPanel.ClientHeight + Offset * 2;

{$IFDEF VCL}
  Width := FColorPanel.Width + Offset * 2;
{$ENDIF VCL}

{$IFDEF VisualCLX}
  // workaround a VisualCLX bug: ClientWidth does not allow values smaller than 100
  Constraints.MaxWidth := FColorPanel.Left + FColorPanel.Width + Offset * 2;

//  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Constraints.MaxWidth;
//  Constraints.MinHeight := Constraints.MaxHeight;
{$ENDIF VisualCLX}

  if FDragBar.Visible then
    FDragBar.SetBounds(Offset, FDragBarSpace + Offset, FColorPanel.Width, FDragBarHeight);

  FColorPanel.SetBounds(Offset, TempHeight + 1,
    FColorPanel.Width, FColorPanel.Height);
  FBusy := False;
end;

procedure TJvOfficeColorForm.SetMeasure(const Index, Value: integer);
var
  MeasureItem: PInteger;
  MeasureConst: integer;
begin
  case Index of
    Tag_DragBarHeight:
      begin
        MeasureItem := @FDragBarHeight;
        MeasureConst := MinDragBarHeight;
      end;
    Tag_DragBarSpace:
      begin
        MeasureItem := @FDragBarSpace;
        MeasureConst := MinDragBarSpace;
      end;
  else
    Exit;
  end;
  if MeasureItem^ = Value then Exit;

  MeasureItem^ := Value;
  FWordStyle := false;
  if MeasureItem^ < MeasureConst then MeasureItem^ := MeasureConst;
  AdjustColorForm();
end;

procedure TJvOfficeColorForm.SetFlat(const Value: boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
  end;
end;

procedure TJvOfficeColorForm.SetToolWindowStyle(const Value: boolean);
begin
  if ShowDragBar then
  begin
    FToolWindowStyle := Value;
{$IFDEF VisualCLX}
    if Value then
    begin
      BorderIcons := [biSystemMenu];
      BorderStyle := fbsToolWindow;
      FDragBar.Visible := False;
    end
    else
    begin
      BorderIcons := [];
      BorderStyle := fbsDialog;
      FDragBar.Visible := true;
    end;

{$ELSE}
    if Value then
    begin
      BorderIcons := [biSystemMenu];
      BorderStyle := bsToolWindow;
      FDragBar.Visible := False;
    end
    else
    begin
      BorderStyle := bsDialog;
      FDragBar.Visible := true;
    end;

{$ENDIF VisualCLX}
    if not DropDownMoving then
      AdjustColorForm();
    if Assigned(FOnWindowStyleChanged) then
      FOnWindowStyleChanged(self);
  end
  else
  begin
    FToolWindowStyle := False;
    BorderIcons := [];
{$IFDEF VisualCLX}
    BorderStyle := fbsDialog;
{$ELSE}
    BorderStyle := bsDialog;
{$ENDIF VisualCLX}
    FDragBar.Visible := False;
  end;
end;

procedure TJvOfficeColorForm.ShowingChanged;
begin
  inherited;
  if Assigned(FOnShowingChanged) then
    FOnShowingChanged(ActiveControl);
end;

procedure TJvOfficeColorForm.DoKillFocus(FocusedWnd: HWND);
begin
  inherited;
  if Assigned(FOnKillFocus) and not DropDownMoving then
    FOnKillFocus(ActiveControl);
end;

const
  sc_DragMove: longint = $F012;

procedure TJvOfficeColorForm.FormDeactivate(Sender: TObject);
begin
  MoveStart := False;
  DropDownMoved := False;
end;

procedure TJvOfficeColorForm.VisibleChanged;
begin
  inherited;
  if not Visible then
  begin
    if ToolWindowStyle then
      ToolWindowStyle := False;

    DropDownMoving := False;
    MoveStart := False;
    DropDownMoved := False;
  end;
end;

{ TJvSubDragBar }

procedure TJvSubDragBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
{$IFDEF VisualCLX}
var
  p: TPoint;
{$ENDIF VisualCLX}
begin
  inherited;
  if Button = mbLeft then
  begin
    TJvOfficeColorForm(OwnerForm).MoveStart := True;
{$IFDEF VisualCLX}
    p := ClientToScreen(Point(x, y));
    Fx := p.X;
    Fy := p.Y;
{$ENDIF VisualCLX}
  end;

end;

procedure TJvSubDragBar.MouseEnter(Control: TControl);
begin
  inherited;
{$IFDEF VisualCLX}
  Color := $996666;
{$ELSE}
  Color := clActiveCaption;
{$ENDIF VisualCLX}
  Cursor := crSizeAll;
end;

procedure TJvSubDragBar.MouseLeave(Control: TControl);
begin
  inherited;
{$IFDEF VisualCLX}
  Color := $999999;
{$ELSE}
  Color := clInactiveCaption;
{$ENDIF VisualCLX}
  Cursor := crDefault;
end;

procedure TJvOfficeColorForm.Resize;
begin
  inherited;
  if FInited then
    AdjustColorForm();
end;

procedure TJvOfficeColorForm.SetWordStyle(const Value: boolean);
begin
  if FWordStyle <> Value then
  begin
    FWordStyle := Value;
    if FWordStyle then
    begin
      FDragBarHeight := MinDragBarHeight;
      FDragBarSpace := MinDragBarSpace;
    end;
  end;
end;

{$IFDEF VCL}

procedure DragControl(WinControl: TWinControl);
const
  SM = $F012;
begin
  ReleaseCapture;
  WinControl.Perform(WM_SYSCOMMAND, SM, 0);
end;
{$ENDIF VCL}

procedure TJvSubDragBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  lOwnerForm: TJvOfficeColorForm;
{$IFDEF VisualCLX}
  p, q: TPoint;
{$ENDIF VisualCLX}
begin
  inherited;
  lOwnerForm := TJvOfficeColorForm(OwnerForm);
  if lOwnerForm.MoveStart or
    lOwnerForm.DropDownMoving then
  begin
    if not lOwnerForm.DropDownMoved then
    begin
      lOwnerForm.DropDownMoved := True;
    end;
{$IFDEF VCL}
    if lOwnerForm.MoveStart and not lOwnerForm.ToolWindowStyle then
    begin
      lOwnerForm.ToolWindowStyle := True;
      lOwnerForm.AdjustColorForm();
    end;
    DragControl(lOwnerForm);
{$ENDIF VCL}

    lOwnerForm.DropDownMoving := True;
    lOwnerForm.MoveStart := false;

{$IFDEF VisualCLX}
    q := ClientToScreen(Point(X, y));
    p := Point(q.x - Fx, q.y - Fy);
    if ((p.x <> 0) or (p.y <> 0)) then
      with lOwnerForm do
      begin
        Left := Left + p.X;
        Top := Top + p.Y;
        Fx := q.x;
        Fy := q.y;
      end;
{$ENDIF VisualCLX}
  end;

end;

procedure TJvSubDragBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
{$IFDEF VisualCLX}
var
  p: TPoint;
{$ENDIF VisualCLX}
begin
  inherited;
  if Button = mbLeft then
  begin
{$IFDEF VisualCLX}
    if TJvOfficeColorForm(OwnerForm).DropDownMoving then
    begin
      TJvOfficeColorForm(OwnerForm).DropDownMoving := False;
      TJvOfficeColorForm(OwnerForm).MoveStart := false;
      TJvOfficeColorForm(OwnerForm).ToolWindowStyle := True;
      p := ClientToScreen(Point(x, y));
      OwnerForm.Top := p.Y + 10;
      Fx := 0;
      Fy := 0;
    end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
    TJvOfficeColorForm(OwnerForm).MoveStart := false;
    TJvOfficeColorForm(OwnerForm).DropDownMoving := False;
{$ENDIF VCL}
  end;

end;

procedure TJvOfficeColorForm.SetShowDragBar(const Value: Boolean);
begin
  FShowDragBar := Value;
  if Value then
  begin
    if not DropDownMoved then
      FDragBar.Visible := True;
  end
  else
  begin
    if DropDownMoved then
    begin
      SetToolWindowStyle(False);
    end;
  end;
  AdjustColorForm;
end;

procedure TJvOfficeColorForm.Paint;
{$IFDEF VisualCLX}
var
  Rec: TRect;
{$ENDIF VisualCLX}
begin
  inherited;
{$IFDEF VisualCLX}
  //in CLx Form have not border,Draw it myself
  if not ToolWindowStyle then
  begin
    Rec := ClientRect;
    Frame3D(Canvas, Rec, clActiveCaption, cl3DDkShadow, 1)
  end;
{$ENDIF VisualCLX}
end;

end.

