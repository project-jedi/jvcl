{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorForm.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Th鰎nqvist [peter3 at sourceforge dot net]
Portions created by Peter Th鰎nqvist are Copyright (C) 2002 Peter Th鰎nqvist.
All Rights Reserved.

Contributor(s):
dejoy(dejoy att ynl dott gov dott cn)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Color form for the @link(TJvColorButton) component

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQOfficeColorForm;

interface

uses
  SysUtils, Classes,
  Types, QWindows, QMessages, QGraphics, QControls, QForms, 
  Qt, QStdCtrls, QExtCtrls, 
  JvQComponent, JvQLabel, JvQOfficeColorPanel;

{------------------------------------------------------------------------------}
const
  MinDragBarHeight = 7;
  MinDragBarSpace = 3;

  Tag_DragBarHeight = 9;
  Tag_DragBarSpace = 10;
  
  JvDefaultSubDragBarActiveColor = TColor($999999);
  JvDefaultSubDragBarInactiveColor = TColor($996666); 

type  
  TJvSubDragBar = class(TLabel) 
  private
    FOwnerForm: TControl; 
    FPos: TPoint; 
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
    FBusy: Boolean;
    FInited: Boolean;
    FWordStyle: Boolean;
    FColorPanel: TJvOfficeColorPanel;
    FDragBar: TJvSubDragBar;
    FOwner: TControl;
    FDragBarSpace: Integer;
    FDragBarHeight: Integer;
    FFlat: Boolean;
    FToolWindowStyle: Boolean;
    FOnShowingChanged: TNotifyEvent;
    FOnKillFocus: TNotifyEvent;
    FOnWindowStyleChanged: TNotifyEvent;
    FShowDragBar: Boolean;
    FDragBarHint: string;
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetMeasure(const Index, Value: Integer);
    procedure SetFlat(const Value: Boolean);
    procedure SetWordStyle(const Value: Boolean);
    procedure SetToolWindowStyle(const Value: Boolean);
    procedure SetShowDragBar(const Value: Boolean);
    procedure SetDragBarHint(const Value: string);
  protected
    DropDownMoved: Boolean; //移动过
    DropDownMoving: Boolean; //正在移动
    MoveEnd: Boolean; //移动完毕
    MoveStart: Boolean; //开始移动
    procedure Resize; override;
    procedure VisibleChanged; override;
    procedure AdjustColorForm;  
    function WidgetFlags: Integer; override;
    procedure Paint; override; 
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
    property DragBarHeight: Integer index Tag_DragBarHeight read FDragBarHeight write SetMeasure;
    property DragBarHint: string read FDragBarHint write SetDragBarHint;
    property DragBarSpace: Integer index Tag_DragBarSpace read FDragBarSpace write SetMeasure;
    property ToolWindowStyle: Boolean read FToolWindowStyle write SetToolWindowStyle default False;
    property Flat: Boolean read FFlat write SetFlat;
  end;

implementation

uses
  JvQResources;

//=== { TJvOfficeColorForm } =================================================

constructor TJvOfficeColorForm.CreateNew(AOwner: TComponent; Dummy: Integer);
var
  ParentControl: TWinControl;
begin
  inherited CreateNew(AOwner, Dummy);
  HintColor := Application.HintColor;
  FInited := False;
  FShowDragBar := True;
//  Scaled := False;
  AutoScroll := False;  
  BorderIcons := [];
  BorderStyle := fbsDialog;
  {$IFDEF MSWINDOWS}
//  Font.Name := 'MS Shell Dlg 2';
  {$ENDIF MSWINDOWS} 
  FormStyle := fsStayOnTop;
  Caption := RsColorWindow;

  FToolWindowStyle := False;
  FDragBarHint := RsDragToFloat;

  ParentControl := Self;

  FDragBar := TJvSubDragBar.Create(Self);
  with FDragBar do
  begin
    Parent := ParentControl;
    FOwnerForm := Self;
    AutoSize := False;
    Caption := '';
    Color := JvDefaultSubDragBarActiveColor;
    Height := MinDragBarHeight;
    ShowHint := True;
    Hint := FDragBarHint;
  end;

  FColorPanel := TJvOfficeColorPanel.Create(Self);
  with FColorPanel do
    Parent := ParentControl;

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




function TJvOfficeColorForm.WidgetFlags: Integer;
begin
  Result := inherited WidgetFlags and
    not Integer(WidgetFlags_WStyle_Title) or
    Integer(WidgetFlags_WType_Popup);
end;


procedure TJvOfficeColorForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Hide;
    ModalResult := mrCancel;
  end;
end;

procedure TJvOfficeColorForm.AdjustColorForm;
var
  TempHeight: Integer;
  HasDragBar: Boolean;
  Offset: Integer;
begin
  if not FInited or FBusy then
    Exit;
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
  
  Offset := 2; 

  if HasDragBar then
    TempHeight := FDragBarHeight + FDragBarSpace * 2
  else
    TempHeight := 0;

  ClientHeight := TempHeight + FColorPanel.ClientHeight + Offset * 2;
 
 
  // workaround a VisualCLX bug: ClientWidth does not allow values smaller than 100
  Constraints.MaxWidth := FColorPanel.Left + FColorPanel.Width + Offset * 2;

//  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Constraints.MaxWidth;
//  Constraints.MinHeight := Constraints.MaxHeight; 

  if FDragBar.Visible then
    FDragBar.SetBounds(Offset, FDragBarSpace + Offset, FColorPanel.Width, FDragBarHeight);

  FColorPanel.SetBounds(Offset, TempHeight + 1, FColorPanel.Width, FColorPanel.Height);
  FBusy := False;
end;

procedure TJvOfficeColorForm.SetMeasure(const Index, Value: Integer);
var
  MeasureItem: PInteger;
  MeasureConst: Integer;
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
  if MeasureItem^ = Value then
    Exit;

  MeasureItem^ := Value;
  FWordStyle := False;
  if MeasureItem^ < MeasureConst then
    MeasureItem^ := MeasureConst;
  AdjustColorForm;
end;

procedure TJvOfficeColorForm.SetFlat(const Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
  end;
end;

procedure TJvOfficeColorForm.SetToolWindowStyle(const Value: Boolean);
begin
  if ShowDragBar then
  begin
    FToolWindowStyle := Value;  
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
      FDragBar.Visible := True;
    end; 
    if not DropDownMoving then
      AdjustColorForm;
    if Assigned(FOnWindowStyleChanged) then
      FOnWindowStyleChanged(Self);
  end
  else
  begin
    FToolWindowStyle := False;
    BorderIcons := [];  
    BorderStyle := fbsDialog; 
    FDragBar.Visible := False;
  end;
end;

procedure TJvOfficeColorForm.ShowingChanged;
begin
  inherited ShowingChanged;
  if Assigned(FOnShowingChanged) then
    FOnShowingChanged(ActiveControl);
end;

procedure TJvOfficeColorForm.DoKillFocus(FocusedWnd: HWND);
begin
  inherited DoKillFocus(FocusedWnd);
  if Assigned(FOnKillFocus) and not DropDownMoving then
    FOnKillFocus(ActiveControl);
end;

procedure TJvOfficeColorForm.FormDeactivate(Sender: TObject);
begin
  MoveStart := False;
  DropDownMoved := False;
end;

procedure TJvOfficeColorForm.VisibleChanged;
begin
  inherited VisibleChanged;
  if not Visible then
  begin
    if ToolWindowStyle then
      ToolWindowStyle := False;

    DropDownMoving := False;
    MoveStart := False;
    DropDownMoved := False;
  end;
end;

procedure TJvOfficeColorForm.SetDragBarHint(const Value: string);
begin
  if FDragBarHint<>Value then
  begin
    FDragBarHint := Value;
    FDragBar.Hint := Value;
  end;
end;

//=== { TJvSubDragBar } ======================================================

procedure TJvSubDragBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    TJvOfficeColorForm(FOwnerForm).MoveStart := True; 
    FPos := ClientToScreen(Point(X, Y)); 
  end;
end;

procedure TJvSubDragBar.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  Color := JvDefaultSubDragBarActiveColor;
  Cursor := crSizeAll;
end;

procedure TJvSubDragBar.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  Color := JvDefaultSubDragBarInactiveColor;
  Cursor := crDefault;
end;

procedure TJvOfficeColorForm.Resize;
begin
  inherited Resize;
  if FInited then
    AdjustColorForm;
end;

procedure TJvOfficeColorForm.SetWordStyle(const Value: Boolean);
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



procedure TJvSubDragBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  lOwnerForm: TJvOfficeColorForm; 
  P, Q: TPoint; 
begin
  inherited MouseMove(Shift, X, Y);
  lOwnerForm := TJvOfficeColorForm(FOwnerForm);
  if lOwnerForm.MoveStart or lOwnerForm.DropDownMoving then
  begin
    if not lOwnerForm.DropDownMoved then
      lOwnerForm.DropDownMoved := True; 

    lOwnerForm.DropDownMoving := True;
    lOwnerForm.MoveStart := False;
 
    Q := ClientToScreen(Point(X, Y));
    P := Point(Q.X - FPos.X, Q.Y - FPos.Y);
    if (P.X <> 0) or (P.Y <> 0) then
      with lOwnerForm do
      begin
        Left := Left + P.X;
        Top := Top + P.Y;
        FPos := Q;
      end; 
  end;
end;

procedure TJvSubDragBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

var
  P: TPoint;

begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin  
    if TJvOfficeColorForm(FOwnerForm).DropDownMoving then
    begin
      TJvOfficeColorForm(FOwnerForm).DropDownMoving := False;
      TJvOfficeColorForm(FOwnerForm).MoveStart := False;
      TJvOfficeColorForm(FOwnerForm).ToolWindowStyle := True;
      P := ClientToScreen(Point(X, Y));
      FOwnerForm.Top := P.Y + 10;
      FPos.X := 0;
      FPos.Y := 0;
    end; 
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
      SetToolWindowStyle(False);
  end;
  AdjustColorForm;
end;


procedure TJvOfficeColorForm.Paint;
var
  Rec: TRect;
begin
  inherited Paint;
  //in Clx Form have no border, paint it
  if not ToolWindowStyle then
  begin
    Rec := ClientRect;
    Frame3D(Canvas, Rec, clActiveCaption, cl3DDkShadow, 1)
  end;
end;


end.

