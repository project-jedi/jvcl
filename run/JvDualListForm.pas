{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFDualLst.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDualListForm;

{$I jvcl.inc}

interface

uses
  Windows,
  {$IFDEF VisualCLX}
  Types,
  {$ENDIF VisualCLX}
  Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  {$IFDEF VCL}
  JvListBox, JvCtrls,
  {$ENDIF VCL}
  JvComponent, JvExStdCtrls;

type
  {$IFDEF VisualCLX}
  TJvListBox = TListBox;
  {$ENDIF VisualCLX}
  TJvDualListForm = class(TJvForm)
    SrcList: TJvListBox;
    DstList: TJvListBox;
    SrcLabel: TLabel;
    DstLabel: TLabel;
    IncBtn: TButton;
    IncAllBtn: TButton;
    ExclBtn: TButton;
    ExclAllBtn: TButton;
    OkBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    Bevel1: TBevel;
    procedure IncBtnClick(Sender: TObject);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExclBtnClick(Sender: TObject);
    procedure ExclAllBtnClick(Sender: TObject);
    procedure SrcListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DstListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SrcListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DstListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SrcListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DstListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListClick(Sender: TObject);
  private
    function GetShowHelp: Boolean;
    procedure SetShowHelp(Value: Boolean);
  {$IFDEF VCL}
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  {$ENDIF VCL}
  public
    procedure SetButtons;
    property ShowHelp: Boolean read GetShowHelp write SetShowHelp default True;
end;

implementation

uses
  Consts,  
  JvBoxProcs;

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

{$IFDEF VCL}
procedure TJvDualListForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;
{$ENDIF VCL}

procedure TJvDualListForm.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := (SrcList.Items.Count = 0);
  DstEmpty := (DstList.Items.Count = 0);
  IncBtn.Enabled := not SrcEmpty and (SrcList.SelCount > 0);
  IncAllBtn.Enabled := not SrcEmpty;
  ExclBtn.Enabled := not DstEmpty and (DstList.SelCount > 0);
  ExclAllBtn.Enabled := not DstEmpty;
end;

function TJvDualListForm.GetShowHelp: Boolean;
begin
  Result := HelpBtn.Enabled and HelpBtn.Visible;
end;

procedure TJvDualListForm.SetShowHelp(Value: Boolean);
const
  x_FrmBtn = 16;
  x_GrpBtn = 15;
  x_BtnBtn = 8;
begin
  with HelpBtn do
  begin
    Enabled := Value;
    Visible := Value;
  end;
  if Value then
  begin
    HelpBtn.Left := Width - HelpBtn.Width - x_FrmBtn;
    CancelBtn.Left := HelpBtn.Left - CancelBtn.Width - x_GrpBtn;
    OkBtn.Left := CancelBtn.Left - OkBtn.Width - x_BtnBtn;;
  end
  else
  begin
    CancelBtn.Left := Width - CancelBtn.Width - x_FrmBtn;
    OkBtn.Left := CancelBtn.Left - OkBtn.Width - x_BtnBtn;;
  end;
end;

procedure TJvDualListForm.IncBtnClick(Sender: TObject);
begin
  BoxMoveSelectedItems(SrcList, DstList);
  SetButtons;
end;

procedure TJvDualListForm.IncAllBtnClick(Sender: TObject);
begin
  BoxMoveAllItems(SrcList, DstList);
  SetButtons;
end;

procedure TJvDualListForm.ExclBtnClick(Sender: TObject);
begin
  BoxMoveSelectedItems(DstList, SrcList);
  SetButtons;
end;

procedure TJvDualListForm.ExclAllBtnClick(Sender: TObject);
begin
  BoxMoveAllItems(DstList, SrcList);
  SetButtons;
end;

procedure TJvDualListForm.SrcListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(SrcList, Source, X, Y, State, Accept, SrcList.Sorted);
  {$IFDEF VCL}
  if State = dsDragLeave then
    (Source as TJvListBox).DragCursor := crDrag;
  if (State = dsDragEnter) and ((Source as TJvListBox).SelCount > 1) then
    (Source as TJvListBox).DragCursor := crMultiDrag;
  {$ENDIF VCL}
end;

procedure TJvDualListForm.DstListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(DstList, Source, X, Y, State, Accept, DstList.Sorted);
  {$IFDEF VCL}
  if State = dsDragLeave then
    (Source as TJvListBox).DragCursor := crDrag;
  if (State = dsDragEnter) and ((Source as TJvListBox).SelCount > 1) then
    (Source as TJvListBox).DragCursor := crMultiDrag;
  {$ENDIF VCL}
end;

procedure TJvDualListForm.SrcListDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  if Source = DstList then
    ExclBtnClick(SrcList)
  else
  if Source = SrcList then
    BoxMoveFocusedItem(SrcList, SrcList.ItemAtPos(Point(X, Y), True));
end;

procedure TJvDualListForm.DstListDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  if Source = SrcList then
    IncBtnClick(DstList)
  else
  if Source = DstList then
    BoxMoveFocusedItem(DstList, DstList.ItemAtPos(Point(X, Y), True));
end;

procedure TJvDualListForm.SrcListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Incr: Integer;
begin
  if not SrcList.Sorted then
  begin
    if (ssCtrl in Shift) and ((Key = VK_DOWN) or (Key = VK_UP)) then
    begin
      if Key = VK_DOWN then
        Incr := 1
      else
        Incr := -1;
      BoxMoveFocusedItem(SrcList, SrcList.ItemIndex + Incr);
      Key := 0;
    end;
  end;
end;

procedure TJvDualListForm.DstListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Incr: Integer;
begin
  if not DstList.Sorted then
  begin
    if (ssCtrl in Shift) and ((Key = VK_DOWN) or (Key = VK_UP)) then
    begin
      if Key = VK_DOWN then
        Incr := 1
      else
        Incr := -1;
      BoxMoveFocusedItem(DstList, DstList.ItemIndex + Incr);
      Key := 0;
    end;
  end;
end;

procedure TJvDualListForm.HelpBtnClick(Sender: TObject);
begin
  {$IFDEF VCL}
  Application.HelpContext(HelpContext);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Application.ContextHelp(HelpContext);
  {$ENDIF VisualCLX}
end;

procedure TJvDualListForm.FormCreate(Sender: TObject);
begin
  OkBtn.Caption := SOKButton;
  CancelBtn.Caption := SCancelButton;
  HelpBtn.Caption := SHelpButton;
  {$IFDEF VCL}
  if NewStyleControls then
    Font.Style := [];
  {$ENDIF VCL}
end;

procedure TJvDualListForm.ListClick(Sender: TObject);
begin
  SetButtons;
end;

end.
