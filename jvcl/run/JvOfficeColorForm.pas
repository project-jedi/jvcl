{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOfficeColorForm.PAS, released on 2004-02-26.

The Initial Developer of the Original Code is dejoy [dejoy att ynl dott gov dott cn]
Portions created by Peter Thörnqvist are Copyright (C) 2004 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):dejoy.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Color form for the @link(TJvOfficeColorButton) component

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvOfficeColorForm;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  JvConsts, JvComponent, JvOfficeColorPanel;

{------------------------------------------------------------------------------}
const
  MinDragBarHeight = 7;
  MinDragBarSpace = 3;

  Tag_DragBarHeight = 9;
  Tag_DragBarSpace = 10;

  JvDefaultSubDragBarActiveColor = clActiveCaption;
  JvDefaultSubDragBarInactiveColor = clInactiveCaption;

type
  TJvOfficeColorPanelDragBar = class(TJvWinControl)
  private
    FOwnerForm: TControl;
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
    FColorPanel: TJvCustomOfficeColorPanel;
    FDragBar: TJvOfficeColorPanelDragBar;
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
    DropDownMoved: Boolean;
    DropDownMoving: Boolean;
    MoveEnd: Boolean;
    MoveStart: Boolean;
    procedure Resize; override;
    procedure VisibleChanged; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure FocusKilled(NextWnd: THandle); override;
    procedure ShowingChanged; override;
    property OnShowingChanged: TNotifyEvent read FOnShowingChanged write FOnShowingChanged;
    property OnKillFocus: TNotifyEvent read FOnKillFocus write FOnKillFocus;
    property OnWindowStyleChanged: TNotifyEvent read FOnWindowStyleChanged write FOnWindowStyleChanged;
  public
    constructor Create(AOwner: TComponent;AOfficeColorPanelClass: TJvOfficeColorPanelClass); reintroduce; virtual;
    procedure AdjustColorForm;
    property ColorPanel: TJvCustomOfficeColorPanel read FColorPanel;
    property ShowDragBar: Boolean read FShowDragBar write SetShowDragBar default True;
    property DragBarHeight: Integer index Tag_DragBarHeight read FDragBarHeight write SetMeasure;
    property DragBarHint: string read FDragBarHint write SetDragBarHint;
    property DragBarSpace: Integer index Tag_DragBarSpace read FDragBarSpace write SetMeasure;
    property ToolWindowStyle: Boolean read FToolWindowStyle write SetToolWindowStyle default False;
    property Flat: Boolean read FFlat write SetFlat;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JvResources;
type
  TJvOfficeColorPanelAccessProtected = class(TJvOfficeColorPanel);

//=== { TJvOfficeColorForm } =================================================

constructor TJvOfficeColorForm.Create(AOwner: TComponent;
  AOfficeColorPanelClass: TJvOfficeColorPanelClass);
var
  ParentControl: TWinControl;
begin
  inherited CreateNew(AOwner);
  HintColor := Application.HintColor;
  FInited := False;
  FShowDragBar := True;
//  Scaled := False;
  AutoScroll := False;
  BorderIcons := [];
  BorderStyle := fbsDialog;
  BorderWidth := 0;
  AutoSize := True;
  FormStyle := fsStayOnTop;

  FToolWindowStyle := False;
  ParentControl := Self;

  FDragBar := TJvOfficeColorPanelDragBar.Create(Self);
  with FDragBar do
  begin
    Parent := ParentControl;
    FOwnerForm := Self;
    AutoSize := False;
    Caption := '';
    Color := JvDefaultSubDragBarActiveColor;
    Height := MinDragBarHeight;
    ShowHint := True;
  end;

  FColorPanel := AOfficeColorPanelClass.Create(Self);
  with TJvOfficeColorPanelAccessProtected(FColorPanel) do
  begin
    Parent := ParentControl;
    FlatBorder := True;
    BorderWidth := 0;
  end;

  SetWordStyle(True);
  KeyPreview := True;
  OnDeactivate := FormDeactivate;
  OnKeyUp := FormKeyUp;

  FInited := True;
end;


procedure TJvOfficeColorForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style and not WS_CAPTION;
  end;
end;




procedure TJvOfficeColorForm.Paint;

begin
  inherited Paint;

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

  Offset := 0;

  if HasDragBar then
    TempHeight := FDragBarHeight + FDragBarSpace * 2
  else
    TempHeight := 0;

  ClientHeight := TempHeight + FColorPanel.ClientHeight + Offset * 2;

  Width := FColorPanel.Width + Offset * 2;


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
      {$IFDEF COMPILER10_UP}
      if HandleAllocated then // BDS 2006 bug
        RecreateWnd;
      {$ENDIF COMPILER10_UP}
      FDragBar.Visible := False;
    end
    else
    begin
      BorderIcons := [];
      BorderStyle := fbsDialog;
      {$IFDEF COMPILER10_UP}
      if HandleAllocated then // BDS 2006 bug
        RecreateWnd;
      {$ENDIF COMPILER10_UP}
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
    {$IFDEF COMPILER10_UP}
    if HandleAllocated then // BDS 2006 bug
      RecreateWnd;
    {$ENDIF COMPILER10_UP}
    FDragBar.Visible := False;
  end;
end;

procedure TJvOfficeColorForm.ShowingChanged;
begin
  inherited ShowingChanged;
  if Assigned(FOnShowingChanged) then
    FOnShowingChanged(ActiveControl);
end;

procedure TJvOfficeColorForm.FocusKilled(NextWnd: THandle);
begin
  inherited FocusKilled(NextWnd);
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

//=== { TJvOfficeColorPanelDragBar } =========================================

procedure TJvOfficeColorPanelDragBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    TJvOfficeColorForm(FOwnerForm).MoveStart := True;
  end;
end;

procedure TJvOfficeColorPanelDragBar.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  Color := JvDefaultSubDragBarActiveColor;
  Cursor := crSizeAll;
end;

procedure TJvOfficeColorPanelDragBar.MouseLeave(Control: TControl);
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



procedure DragControl(WinControl: TWinControl);
const
  SM = $F012;
begin
  ReleaseCapture;
  WinControl.Perform(WM_SYSCOMMAND, SM, 0);
end;



procedure TJvOfficeColorPanelDragBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  lOwnerForm: TJvOfficeColorForm;
begin
  inherited MouseMove(Shift, X, Y);
  lOwnerForm := TJvOfficeColorForm(FOwnerForm);
  if lOwnerForm.MoveStart or lOwnerForm.DropDownMoving then
  begin
    if not lOwnerForm.DropDownMoved then
      lOwnerForm.DropDownMoved := True;
    if lOwnerForm.MoveStart and not lOwnerForm.ToolWindowStyle then
    begin
      lOwnerForm.ToolWindowStyle := True;
      lOwnerForm.AdjustColorForm;
    end;
    DragControl(lOwnerForm);

    lOwnerForm.DropDownMoving := True;
    lOwnerForm.MoveStart := False;

  end;
end;

procedure TJvOfficeColorPanelDragBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    TJvOfficeColorForm(FOwnerForm).MoveStart := False;
    TJvOfficeColorForm(FOwnerForm).DropDownMoving := False;
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

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

