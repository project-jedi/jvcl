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

The Original Code is: JvSbSetup.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQSpeedbarSetupForm;

{$I jvcl.inc}

interface

uses
  SysUtils,  Classes, 
  QGraphics, QControls, QForms, QStdCtrls, QGrids, QExtCtrls, 
  QWindows, 
  JvQConsts, JvQSpeedBar, JvQSpeedButton, JvQComponent;

type
  TJvSpeedbarSetupWindow = class(TJvForm)
    ButtonsList: TDrawGrid;
    ButtonsLabel: TLabel;
    SectionList: TDrawGrid;
    CategoriesLabel: TLabel;
    Bevel1: TBevel;
    HintLabel: TLabel;
    CloseBtn: TButton;
    HelpBtn: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SectionListSelectCell(Sender: TObject; Col, Row: Longint;
      var CanSelect: Boolean);
    procedure SectionListDrawCell(Sender: TObject; Col, Row: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure ButtonsListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonsListMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ButtonsListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonsListSelectCell(Sender: TObject; Col, Row: Longint;
      var CanSelect: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonsListDrawCell(Sender: TObject; Col, Row: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure CloseBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FButton: TJvBtnControl;
    FImage: TJvButtonImage;
    FSpeedbar: TJvSpeedBar;
    FDrag: Boolean;
    FDragItem: TJvSpeedItem;
    procedure UpdateHint(Section, Row: Integer);
    function CheckSpeedBar: Boolean;
    function CurrentSection: Integer;
    procedure SetSection(Section: Integer);
    procedure UpdateCurrentSection;
    procedure UpdateData(Section: Integer);
    procedure UpdateListHeight;
    procedure SetSpeedbar(Value: TJvSpeedBar);
    function ItemByRow(Row: Integer): TJvSpeedItem;
    procedure CMSpeedBarChanged(var Msg: TMessage); message CM_SPEEDBARCHANGED;
  public
    property Speedbar: TJvSpeedBar read FSpeedbar write SetSpeedbar;
  end;

procedure ShowSpeedbarSetupWindow(Speedbar: TJvSpeedBar; HelpCtx: THelpContext);

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math, QConsts,
  JvQJVCLUtils, JvQTypes, JvQResources;

{$R *.xfm}

function FindEditor(Speedbar: TJvSpeedBar): TJvSpeedbarSetupWindow;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I] is TJvSpeedbarSetupWindow then
      if TJvSpeedbarSetupWindow(Screen.Forms[I]).Speedbar = Speedbar then
      begin
        Result := TJvSpeedbarSetupWindow(Screen.Forms[I]);
        Break;
      end;
end;

procedure ShowSpeedbarSetupWindow(Speedbar: TJvSpeedBar; HelpCtx: THelpContext);
var
  Editor: TJvSpeedbarSetupWindow;
begin
  if Speedbar = nil then
    Exit;
  Editor := FindEditor(Speedbar);
  if Editor = nil then
  begin
    Editor := TJvSpeedbarSetupWindow.Create(Application);
    Editor.Speedbar := Speedbar;
  end;
  try
    if HelpCtx > 0 then
      Editor.HelpContext := HelpCtx;
    Editor.BorderIcons := [biSystemMenu];
    Editor.HelpBtn.Visible := (HelpCtx > 0);
    Editor.Show;
    if Editor.WindowState = wsMinimized then
      Editor.WindowState := wsNormal;
  except
    Editor.Free;
    raise;
  end;
end;

const
  MaxBtnListHeight = 186;

function TJvSpeedbarSetupWindow.CheckSpeedBar: Boolean;
begin
  Result := (FSpeedbar <> nil) and (FSpeedbar.Owner <> nil) and (FSpeedbar.Parent <> nil);
end;

function TJvSpeedbarSetupWindow.CurrentSection: Integer;
begin
  if CheckSpeedBar and (FSpeedbar.SectionCount > 0) then
    Result := SectionList.Row
  else
    Result := -1;
end;

procedure TJvSpeedbarSetupWindow.SetSection(Section: Integer);
var
  I: Integer;
begin
  if CheckSpeedBar then
  begin
    I := Section;
    if (I >= 0) and (FSpeedbar.SectionCount > 0) then
      ButtonsList.RowCount := FSpeedbar.ItemsCount(I)
    else
      ButtonsList.RowCount := 0;
    SectionList.DefaultColWidth := SectionList.ClientWidth;
    ButtonsList.DefaultColWidth := ButtonsList.ClientWidth;
    UpdateHint(I, ButtonsList.Row);
  end;
end;

procedure TJvSpeedbarSetupWindow.UpdateCurrentSection;
begin
  SetSection(CurrentSection);
end;

procedure TJvSpeedbarSetupWindow.UpdateData(Section: Integer);
begin
  if CheckSpeedBar then
  begin
    SectionList.RowCount := FSpeedbar.SectionCount;
    UpdateCurrentSection;
    if (Section >= 0) and (Section < SectionList.RowCount) then
      SectionList.Row := Section;
  end
  else
  begin
    SectionList.RowCount := 0;
    ButtonsList.RowCount := 0;
  end;
end;

procedure TJvSpeedbarSetupWindow.UpdateListHeight;
var
  Cnt: Integer;
  MaxHeight: Integer;
begin
  Canvas.Font := Font;
  MaxHeight := MulDiv(MaxBtnListHeight, Screen.PixelsPerInch, 96);
  ButtonsList.DefaultRowHeight := FSpeedbar.BtnHeight + 2;
  Cnt := Max(1, Max(ButtonsList.ClientHeight, MaxHeight) div
    (FSpeedbar.BtnHeight + 2));
  ButtonsList.ClientHeight := Min(MaxHeight,
    ButtonsList.DefaultRowHeight * Cnt);
  SectionList.ClientHeight := ButtonsList.ClientHeight;
  SectionList.DefaultRowHeight := CanvasMaxTextHeight(Canvas) + 2;
end;

procedure TJvSpeedbarSetupWindow.SetSpeedbar(Value: TJvSpeedBar);
begin
  if FSpeedbar <> Value then
  begin
    if FSpeedbar <> nil then
      FSpeedbar.SetEditing(NullHandle);
    FSpeedbar := Value;
    if FSpeedbar <> nil then
    begin
      FSpeedbar.SetEditing(Handle);
      UpdateListHeight;
    end;
    UpdateData(-1);
  end;
end;

procedure TJvSpeedbarSetupWindow.CMSpeedBarChanged(var Msg: TMessage);
begin
  if Pointer(Msg.LParam) = FSpeedbar then
    case Msg.WParam of
      SBR_CHANGED:
        UpdateData(CurrentSection);
      SBR_DESTROYED:
        Close;
      SBR_BTNSIZECHANGED:
        if FSpeedbar <> nil then
          UpdateListHeight;
    end;
end;

function TJvSpeedbarSetupWindow.ItemByRow(Row: Integer): TJvSpeedItem;
begin
  Result := FSpeedbar.Items(CurrentSection, Row);
end;

procedure TJvSpeedbarSetupWindow.UpdateHint(Section, Row: Integer);
var
  Item: TJvSpeedItem;
begin
  Item := FSpeedbar.Items(Section, Row);
  if Item <> nil then
    Hint := Item.Hint
  else
    Hint := '';
end;

procedure TJvSpeedbarSetupWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FButton.Free;
  FButton := nil;
  if FSpeedbar <> nil then
    FSpeedbar.SetEditing(NullHandle);
  FSpeedbar := nil;
end;

procedure TJvSpeedbarSetupWindow.SectionListSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  CanSelect := False;
  SetSection(Row);
  CanSelect := True;
end;

procedure TJvSpeedbarSetupWindow.SectionListDrawCell(Sender: TObject;
  Col, Row: Longint; Rect: TRect; State: TGridDrawState);
begin
  if CheckSpeedBar then
    if Row < FSpeedbar.SectionCount then
      DrawCellText(Sender as TDrawGrid, Col, Row,
        FSpeedbar.Sections[Row].Caption, Rect, taLeftJustify, vaCenterJustify,  
          False); 
end;

procedure TJvSpeedbarSetupWindow.ButtonsListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: TJvSpeedItem;
begin
  Item := ItemByRow(ButtonsList.Row);
  if (Item <> nil) and (X < FSpeedbar.BtnWidth + 2) and (Button = mbLeft) then
  begin
    FDrag := True;
    if Item.Visible then
      FDragItem := nil
    else
    begin
      FDragItem := Item;
      if FButton = nil then
      begin
        FButton := TJvBtnControl.Create(Self);
        FButton.AssignSpeedItem(Item);
      end;
    end;
  end;
end;

procedure TJvSpeedbarSetupWindow.ButtonsListMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if FDrag and (FButton <> nil) and (FDragItem <> nil) then
  begin
    P := (Sender as TControl).ClientToScreen(Point(X, Y));
    X := P.X - FButton.Width {div 2};
    Y := P.Y - FButton.Height {div 2};
    FButton.Activate(Bounds(X, Y, FSpeedbar.BtnWidth, FSpeedbar.BtnHeight));
  end
  else
  if FDrag then
    SetCursor(Screen.Cursors[crNoDrop]);
end;

procedure TJvSpeedbarSetupWindow.ButtonsListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if FDrag and (Button = mbLeft) then
  try
    if (FDragItem <> nil) and (FButton <> nil) then
    begin
      Dec(X, FButton.Width {div 2});
      Dec(Y, FButton.Height {div 2});
      P := (Sender as TControl).ClientToScreen(Point(X, Y));
      FButton.Free;
      FButton := nil;
      if CheckSpeedBar and (FSpeedbar = FindSpeedBar(P)) then
      begin
        P := FSpeedbar.ScreenToClient(P);
        if FSpeedbar.AcceptDropItem(FDragItem, P.X, P.Y) then
          UpdateCurrentSection;
      end;
    end
    else
      SetCursor(Screen.Cursors[ButtonsList.Cursor]);
  finally
    FDrag := False;
    FDragItem := nil;
  end;
end;

procedure TJvSpeedbarSetupWindow.ButtonsListSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  CanSelect := not FDrag or (Row = ButtonsList.Row);
  if CanSelect then
    UpdateHint(CurrentSection, Row)
  else
    Hint := '';
end;

procedure TJvSpeedbarSetupWindow.FormCreate(Sender: TObject);
begin
  FImage := TJvButtonImage.Create;
  FButton := nil;
  FSpeedbar := nil;
  FDrag := False;
  CloseBtn.Default := False;
  if NewStyleControls then
    Font.Style := [];
  { Load string resources }
  CloseBtn.Caption := SOKButton;
  HelpBtn.Caption := SHelpButton;
  Caption := RsCustomizeSpeedbar;
  CategoriesLabel.Caption := RsSpeedbarCategories;
  ButtonsLabel.Caption := RsAvailButtons;
  HintLabel.Caption := RsSpeedbarEditHint;
end;

procedure TJvSpeedbarSetupWindow.FormDestroy(Sender: TObject);
begin
  FImage.Free;
end;

procedure TJvSpeedbarSetupWindow.ButtonsListDrawCell(Sender: TObject;
  Col, Row: Longint; Rect: TRect; State: TGridDrawState);
var
  I: Integer;
begin
  I := CurrentSection;
  if (I >= 0) and (Row < FSpeedbar.ItemsCount(I)) then
    DrawCellButton(Sender as TDrawGrid, Rect, ItemByRow(Row), FImage,  
      False); 
end;

procedure TJvSpeedbarSetupWindow.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TJvSpeedbarSetupWindow.HelpBtnClick(Sender: TObject);
begin  
  Application.HelpSystem.ShowContextHelp(HelpContext, Application.HelpFile); 
end;

procedure TJvSpeedbarSetupWindow.FormShow(Sender: TObject);
begin
  if FSpeedbar <> nil then
    UpdateListHeight;
  SectionList.DefaultColWidth := SectionList.ClientWidth;
  ButtonsList.DefaultColWidth := ButtonsList.ClientWidth;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

