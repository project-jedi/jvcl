{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxSbSetup.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://JVCL.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JvxSbSetup;

interface

uses
{$IFDEF WIN32}
  Windows,
{$ELSE}
  WinTypes, WinProcs,
{$ENDIF WIN32}
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Grids, JvxCtrls, JvxSpeedbar, ExtCtrls, JvxConst;

type
  TJvxSpeedbarSetupWindow = class(TForm)
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
    procedure ButtonsListMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
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
    { Private declarations }
    FButton: TJvxBtnControl;
    FImage: TJvxButtonImage;
    FBar: TJvxSpeedBar;
    FDrag: Boolean;
    FDragItem: TJvxSpeedItem;
    procedure UpdateHint(Section, Row: Integer);
    function CheckSpeedBar: Boolean;
    function CurrentSection: Integer;
    procedure SetSection(Section: Integer);
    procedure UpdateCurrentSection;
    procedure UpdateData(Section: Integer);
    procedure UpdateListHeight;
    procedure SeTJvxSpeedBar(Value: TJvxSpeedBar);
    function ItemByRow(Row: Integer): TJvxSpeedItem;
    procedure CMSpeedBarChanged(var Message: TMessage); message CM_SPEEDBARCHANGED;
  public
    { Public declarations }
    property JvxSpeedbar: TJvxSpeedBar read FBar write SeTJvxSpeedBar;
  end;

procedure ShowSpeedbarSetupWindow(JvxSpeedbar: TJvxSpeedBar; HelpCtx: THelpContext);

implementation

uses JvxVCLUtils, JvxMaxMin, Consts, JvxTConst;

{$R *.DFM}

function FindEditor(JvxSpeedbar: TJvxSpeedBar): TJvxSpeedbarSetupWindow;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[I] is TJvxSpeedbarSetupWindow then begin
      if TJvxSpeedbarSetupWindow(Screen.Forms[I]).JvxSpeedbar = JvxSpeedbar then
      begin
        Result := TJvxSpeedbarSetupWindow(Screen.Forms[I]);
        Break;
      end;
    end;
  end;
end;

procedure ShowSpeedbarSetupWindow(JvxSpeedbar: TJvxSpeedBar; HelpCtx: THelpContext);
var
  Editor: TJvxSpeedbarSetupWindow;
begin
  if JvxSpeedbar = nil then Exit;
  Editor := FindEditor(JvxSpeedbar);
  if Editor = nil then begin
    Editor := TJvxSpeedbarSetupWindow.Create(Application);
    Editor.JvxSpeedbar := JvxSpeedbar;
  end;
  try
    if HelpCtx > 0 then Editor.HelpContext := HelpCtx;
{$IFDEF WIN32}
    Editor.BorderIcons := [biSystemMenu];
{$ENDIF}
    Editor.HelpBtn.Visible := (HelpCtx > 0);
    Editor.Show;
    if Editor.WindowState = wsMinimized then Editor.WindowState := wsNormal;
  except
    Editor.Free;
    raise;
  end;
end;

{ TJvxSpeedbarSetupWindow }

const
  MaxBtnListHeight = 186;

function TJvxSpeedbarSetupWindow.CheckSpeedBar: Boolean;
begin
  Result := (FBar <> nil) and (FBar.Owner <> nil) and
    (FBar.Parent <> nil);
end;

function TJvxSpeedbarSetupWindow.CurrentSection: Integer;
begin
  if CheckSpeedBar and (FBar.SectionCount > 0) then
    Result := SectionList.Row
  else Result := -1;
end;

procedure TJvxSpeedbarSetupWindow.SetSection(Section: Integer);
var
  I: Integer;
begin
  if CheckSpeedBar then begin
    I := Section;
    if (I >= 0) and (FBar.SectionCount > 0) then
      ButtonsList.RowCount := FBar.ItemsCount(I)
    else ButtonsList.RowCount := 0;
    SectionList.DefaultColWidth := SectionList.ClientWidth;
    ButtonsList.DefaultColWidth := ButtonsList.ClientWidth;
    UpdateHint(I, ButtonsList.Row);
  end;
end;

procedure TJvxSpeedbarSetupWindow.UpdateCurrentSection;
begin
  SetSection(CurrentSection);
end;

procedure TJvxSpeedbarSetupWindow.UpdateData(Section: Integer);
begin
  if CheckSpeedBar then begin
    SectionList.RowCount := FBar.SectionCount;
    UpdateCurrentSection;
    if (Section >= 0) and (Section < SectionList.RowCount) then
      SectionList.Row := Section;
  end
  else begin
    SectionList.RowCount := 0;
    ButtonsList.RowCount := 0;
  end;
end;

procedure TJvxSpeedbarSetupWindow.UpdateListHeight;
var
  Cnt: Integer;
  MaxHeight: Integer;
begin
  Canvas.Font := Font;
  MaxHeight := MulDiv(MaxBtnListHeight, Screen.PixelsPerInch, 96);
  ButtonsList.DefaultRowHeight := FBar.BtnHeight + 2;
  Cnt := Max(1, Max(ButtonsList.ClientHeight, MaxHeight) div
    (FBar.BtnHeight + 2));
  ButtonsList.ClientHeight := Min(MaxHeight,
    ButtonsList.DefaultRowHeight * Cnt);
  SectionList.ClientHeight := ButtonsList.ClientHeight;
  SectionList.DefaultRowHeight := Canvas.TextHeight('Wg') + 2;
end;

procedure TJvxSpeedbarSetupWindow.SeTJvxSpeedBar(Value: TJvxSpeedBar);
begin
  if FBar <> Value then begin
    if FBar <> nil then FBar.SetEditing(0);
    FBar := Value;
    if FBar <> nil then begin
      FBar.SetEditing(Handle);
      UpdateListHeight;
    end;
    UpdateData(-1);
  end;
end;

procedure TJvxSpeedbarSetupWindow.CMSpeedBarChanged(var Message: TMessage);
begin
  if Pointer(Message.LParam) = FBar then begin
    case Message.WParam of
      SBR_CHANGED: UpdateData(CurrentSection);
      SBR_DESTROYED: Close;
      SBR_BTNSIZECHANGED: if FBar <> nil then UpdateListHeight;
    end;
  end;
end;

function TJvxSpeedbarSetupWindow.ItemByRow(Row: Integer): TJvxSpeedItem;
begin
  Result := FBar.Items(CurrentSection, Row);
end;

procedure TJvxSpeedbarSetupWindow.UpdateHint(Section, Row: Integer);
var
  Item: TJvxSpeedItem;
begin
  Item := FBar.Items(Section, Row);
  if Item <> nil then Hint := Item.Hint
  else Hint := '';
end;

procedure TJvxSpeedbarSetupWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FButton.Free;
  FButton := nil;
  if FBar <> nil then FBar.SetEditing(0);
  FBar := nil;
end;

procedure TJvxSpeedbarSetupWindow.SectionListSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  CanSelect := False;
  SetSection(Row);
  CanSelect := True;
end;

procedure TJvxSpeedbarSetupWindow.SectionListDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);
begin
  if CheckSpeedBar then begin
    if Row < FBar.SectionCount then begin
      DrawCellText(Sender as TDrawGrid, Col, Row,
        FBar.Sections[Row].Caption, Rect, taLeftJustify, vaCenter
        {$IFDEF Delphi4_Up}, TDrawGrid(Sender).IsRightToLeft {$ENDIF});
    end;
  end;
end;

procedure TJvxSpeedbarSetupWindow.ButtonsListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: TJvxSpeedItem;
begin
  Item := ItemByRow(ButtonsList.Row);
  if (Item <> nil) and (X < FBar.BtnWidth + 2) and (Button = mbLeft) then
  begin
    FDrag := True;
    if Item.Visible then FDragItem := nil
    else begin
      FDragItem := Item;
      if FButton = nil then begin
        FButton := TJvxBtnControl.Create(Self);
        FButton.AssignSpeedItem(Item);
      end;
    end;
  end;
end;

procedure TJvxSpeedbarSetupWindow.ButtonsListMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if FDrag and (FButton <> nil) and (FDragItem <> nil) then begin
    P := (Sender as TControl).ClientToScreen(Point(X, Y));
    X := P.X - (FButton.Width {div 2});
    Y := P.Y - (FButton.Height {div 2});
    FButton.Activate(Bounds(X, Y, FBar.BtnWidth, FBar.BtnHeight));
  end
  else if FDrag then SetCursor(Screen.Cursors[crNoDrop]);
end;

procedure TJvxSpeedbarSetupWindow.ButtonsListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if FDrag and (Button = mbLeft) then
  try
    if (FDragItem <> nil) and (FButton <> nil) then begin
      Dec(X, FButton.Width {div 2});
      Dec(Y, FButton.Height {div 2});
      P := (Sender as TControl).ClientToScreen(Point(X, Y));
      FButton.Free;
      FButton := nil;
      if CheckSpeedBar and (FBar = FindSpeedBar(P)) then begin
        P := FBar.ScreenToClient(P);
        if FBar.AcceptDropItem(FDragItem, P.X, P.Y) then
          UpdateCurrentSection;
      end;
    end
    else SetCursor(Screen.Cursors[ButtonsList.Cursor]);
  finally
    FDrag := False;
    FDragItem := nil;
  end;
end;

procedure TJvxSpeedbarSetupWindow.ButtonsListSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  CanSelect := not FDrag or (Row = ButtonsList.Row);
  if CanSelect then UpdateHint(CurrentSection, Row)
  else Hint := '';
end;

procedure TJvxSpeedbarSetupWindow.FormCreate(Sender: TObject);
begin
  FImage := TJvxButtonImage.Create;
  FButton := nil;
  FBar := nil;
  FDrag := False;
  CloseBtn.Default := False;
  if NewStyleControls then Font.Style := [];
  { Load string resources }
  CloseBtn.Caption := ResStr(SOKButton);
  HelpBtn.Caption := ResStr(SHelpButton);
  Caption := LoadStr(SCustomizeSpeedbar);
  CategoriesLabel.Caption := LoadStr(SSpeedbarCategories);
  ButtonsLabel.Caption := LoadStr(SAvailButtons);
  HintLabel.Caption := LoadStr(SSpeedbarEditHint);
end;

procedure TJvxSpeedbarSetupWindow.FormDestroy(Sender: TObject);
begin
  FImage.Free;
end;

procedure TJvxSpeedbarSetupWindow.ButtonsListDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);
var
  I: Integer;
begin
  I := CurrentSection;
  if (I >= 0) and (Row < FBar.ItemsCount(I)) then
    DrawCellButton(Sender as TDrawGrid, Rect, ItemByRow(Row), FImage
      {$IFDEF Delphi4_Up}, TDrawGrid(Sender).IsRightToLeft {$ENDIF});
end;

procedure TJvxSpeedbarSetupWindow.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TJvxSpeedbarSetupWindow.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TJvxSpeedbarSetupWindow.FormShow(Sender: TObject);
begin
  if FBar <> nil then UpdateListHeight;
  SectionList.DefaultColWidth := SectionList.ClientWidth;
  ButtonsList.DefaultColWidth := ButtonsList.ClientWidth;
end;

end.
