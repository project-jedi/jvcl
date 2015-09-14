{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBGridSelectColumnForm.PAS, released on 2004-01-15.

The Initial Developers of the Original Code is Lionel Reynaud
Copyright (c) 2004 Lionel Reynaud
All Rights Reserved.

Contributor(s):
Frank Jepsen added support for column moving in scGrid mode

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBGridSelectColumnForm;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Graphics, Classes, Controls, Forms, StdCtrls, Dialogs, CheckLst, ExtCtrls,
  DB, DBGrids, JvDBGrid, JvComponent;

type
  TfrmSelectColumn = class(TJvForm)
    Panel1: TPanel;
    clbList: TCheckListBox;
    cbWithFieldName: TCheckBox;
    ButtonOK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure cbClick(Sender: TObject);
    procedure clbListClickCheck(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
    procedure clbListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure clbListClick(Sender: TObject);
    procedure clbListStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure clbListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure clbListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    FDataSource: TDataSource;
    FJvDBGrid: TJvDBGrid;
    FSelectColumn: TSelectColumn;
    FColumnUpdate: Boolean;
    FCanHide: Boolean;
    FNoSelectionWarning: string;
    FListindex: Integer;
    FLastScroll: Cardinal;
    FActiveIndex: Integer;
    FLastIndex: Integer;
    FEndDrag: Boolean;
    procedure ResizeForm;
    function GetColumn(AField: TField): TColumn;
  public
    property DataSource: TDataSource read FDataSource write FDataSource;
    property Grid: TJvDBGrid read FJvDBGrid write FJvDBGrid;
    property SelectColumn: TSelectColumn read FSelectColumn write FSelectColumn;
  published
    // make this published so localization tools have a chance to pick it up
    property NoSelectionWarning: string read FNoSelectionWarning write FNoSelectionWarning;
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
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  {$IFDEF RTL240_UP}
  System.Generics.Collections,  // required for TShadowedCollection
  {$ENDIF RTL240_UP}
  SysUtils, Types,
  JvJCLUtils, JvConsts, Math;

{$R *.dfm}

var
  FCheckWidth, FCheckHeight: Integer;

procedure GetCheckSize;
begin
  with TBitmap.Create do
    try
      Handle := LoadBitmap(0, PChar(OBM_CHECKBOXES));
      FCheckWidth := Width div 4 + 2;
      FCheckHeight := Height div 3;
    finally
      Free;
    end;
end;

procedure TfrmSelectColumn.clbListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  n : Integer;
  SourceSel : array of Boolean;
begin
  with Sender as TCheckListBox do
  begin
    // Allow column moving only in scGrid Mode
    if (FSelectColumn = scGrid) and (dgColumnResize in FJvDBGrid.Options) and
      ((Key = VK_DOWN) or (Key = VK_UP)) and (Shift = [ssCtrl]) then
    begin
      if (Key = VK_DOWN) and not Selected[Count - 1] then
      begin
        // Disable redraw
        Self.Perform(WM_SETREDRAW, 0, 0);
        // Save selected
        SetLength(SourceSel,Count);
        for n := 0 to Count -1 do
          SourceSel[n] := Selected[n];
        // Move lines
        for n := Count -1 downto 0 do
          if SourceSel[n] then Items.Move(n, n + 1);
        // Set selected
        Selected[0] := False;
        for n := 0 to Count -1 do
          Selected[n] := SourceSel[n - 1];
        // Enable redraw
        Self.Perform(WM_SETREDRAW, 1, 0);
        Repaint;
      end
      else if (Key = VK_UP) and not Selected[0] then
      begin
        // Disable redraw
        Self.Perform(WM_SETREDRAW, 0, 0);
        // Save selected
        SetLength(SourceSel,Count);
        for n := 0 to Count -1 do
          SourceSel[n] := Selected[n];
        // Move lines
        for n := 0 to Count -1 do
          if SourceSel[n] then Items.Move(n, n - 1);
        // Set selected
        Selected[Count - 1] := False;
        for n := 0 to Count - 2 do
          Selected[n] := SourceSel[n + 1];
        // Enable redraw
        Self.Perform(WM_SETREDRAW, 1, 0);
        Repaint;
      end;
      Key := 0;
    end;
  end;
end;

procedure TfrmSelectColumn.clbListClick(Sender: TObject);
begin
  // Keep track of ItemIndex
  FLastIndex := FActiveIndex;
  FActiveIndex := clbList.ItemIndex;
end;

procedure TfrmSelectColumn.clbListStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
  P: TPoint;
  Index: Integer;
begin
  GetCursorPos(P);
  P := clbList.ScreenToClient(P);
  Index := clbList.ItemAtPos(P,True);
  if (Index <> -1) and clbList.ItemEnabled[Index] and
    (FSelectColumn = scGrid) and (dgColumnResize in FJvDBGrid.Options) then
    if P.X - clbList.ItemRect(Index).Left < FCheckWidth then
    begin
      // Original code will not change Checked when clicking on a selected item
      if Index = FLastIndex then  // Change Checked
        clbList.Checked[Index] := not clbList.Checked[Index];
      FEndDrag := True;           // Do not drag when over checkbox
    end
    else
      FEndDrag := False;          // otherwise drag is ok
end;

procedure TfrmSelectColumn.clbListDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  n : Integer;
  offset : Integer;
  maxoffset : Integer;
  minoffset : Integer;
  SourceSel : array of Boolean;
begin
  if FEndDrag or       // began dragging over checkbox
    not ((FSelectColumn = scGrid) and (dgColumnResize in FJvDBGrid.Options)) then
  begin
    FEndDrag := False;
    clbList.EndDrag(False);
  end
  else if (Sender = Source) and (Sender is TCustomListbox) and ((Source as TCustomListbox).ItemIndex <> -1) and
    ((Source as TCustomListbox).ItemAtPos(point(x, y), True) <> -1) then
  begin
    with Source as TCheckListBox do
    begin
      if (GetTickCount - FLastScroll > 100) then // Slows scrolling to 10 per sec
      begin
        FLastScroll := GetTickCount;
        minoffset := 0;
        maxoffset := 0;
        // Save selected and calc max offset
        SetLength(SourceSel,Count);
        for n := 0 to Count -1 do
        begin
          SourceSel[n] := Selected[n];
          if SourceSel[n] then maxoffset := n - Count;
        end;

        // Calc min offset
        for n := Count -1 downto 0 do
          if SourceSel[n] then minoffset := n ;

        // Autoscroll up and down
        if (y < ItemHeight div 1) and (TopIndex > 0) then
          TopIndex := TopIndex - 1;
        if (y > ClientHeight div ItemHeight * ItemHeight - ItemHeight div 1 - 1) and
          (TopIndex < Count - ClientHeight div ItemHeight) then
          TopIndex := TopIndex + 1;

        // calc movement offset
        offset:= FListindex - ItemAtPos(point(x, y), True);
        if (offset <= maxoffset) then offset := maxoffset + 1;
        if (offset > minoffset) then offset := minoffset;

        if offset <> 0 then
        begin
          // Move lines
          if offset > 0 then
          begin
            // Scroll if neccessary
            if minoffset - offset < TopIndex then
              TopIndex := minoffset - offset;
            Self.Perform(WM_SETREDRAW, 0, 0);    // Disable redraw
            for n := 0 to Count -1 do
              if SourceSel[n] then Items.Move(n, n - offset);
          end
          else if offset < 0 then
          begin
            // Scroll if neccessary
            if Count + maxoffset - offset >= TopIndex + ClientHeight div ItemHeight then
              TopIndex := Count + maxoffset - offset - ClientHeight div ItemHeight + 1;
            Self.Perform(WM_SETREDRAW, 0, 0);    // Disable redraw
            for n := Count -1 downto 0 do
              if SourceSel[n] then Items.Move(n, n - offset);
          end;

          // Set selected
          for n := 0 to Count -1 do
            if SourceSel[n] then Selected[n - offset] := True;

          // Set new itemindex
          FListindex := FListindex - offset;
          ItemIndex := FListindex;
          FActiveIndex := FListindex;
          FLastIndex := FListindex;

          Self.Perform(WM_SETREDRAW, 1, 0);      // Enable redraw
          Repaint;
        end;
      end; 
    end;   //with listbox
    Accept:= true
  end
  else Accept:= false;
end;

procedure TfrmSelectColumn.clbListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  with Sender as TCustomListBox do
    FListindex := ItemAtPos(point(x,y),true);
end;

procedure TfrmSelectColumn.FormCreate(Sender: TObject);
begin
  FColumnUpdate := True;
  FCanHide := True;
  // (p3) don't use resourcestring here since this property is normally set from the JvDBGrid
  // and using resourcestrings might give problems with localization synchronizing
  NoSelectionWarning := 'At least one column must be visible!';
end;

type
{$HINTS OFF}
  TShadowedCollection = class(TPersistent)
  private
    FItemClass: TCollectionItemClass;
    FItems: TList{$IFDEF RTL240_UP}<TCollectionItem>{$ENDIF RTL240_UP};
  end;
{$HINTS ON}

procedure TfrmSelectColumn.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  I, J: Integer;
  List: TList;
begin
  if (ModalResult = mrOk) and FColumnUpdate and FCanHide and Assigned(FJvDBGrid) then
  begin
    FJvDBGrid.BeginUpdate;
    List := TList.Create;
    try
      // Make a copy of all items
      List.Capacity := TShadowedCollection(FJvDBGrid.Columns).FItems.Count;
      for I := 0 to FJvDBGrid.Columns.Count - 1 do
      begin
        List.Add(TShadowedCollection(FJvDBGrid.Columns).FItems[I]);
      end;
      for I := 0 to clbList.Items.Count - 1 do
      begin
        J := Integer(clbList.Items.Objects[I]);
        if (J >= 0) and (J < FJvDBGrid.Columns.Count) then
          FJvDBGrid.Columns[J].Visible := clbList.Checked[I];
        // Sort moved items to new position
        TShadowedCollection(FJvDBGrid.Columns).FItems[I] := List.Items[J];
      end;
    finally
      List.Free;
      FJvDBGrid.EndUpdate;
    end;
  end;
end;

procedure TfrmSelectColumn.FormDestroy(Sender: TObject);
begin
  clbList.Items.Clear;
end;

procedure TfrmSelectColumn.FormActivate(Sender: TObject);
var
  I, J: Integer;
  ColumnTitle: string;
  lColumn: TColumn;
begin
  if Assigned(FJvDBGrid) then
    with FJvDBGrid do
    begin
      clbList.Items.Clear;
      cbWithFieldName.Hide;
      if (FSelectColumn = scDatabase) and Assigned(DataSource) and Assigned(DataSource.Dataset) then
      begin
        with DataSource.Dataset do
          for I := 0 to FieldCount - 1 do
          begin
            lColumn := GetColumn(Fields[I]);
            if Assigned(lColumn) then
            begin
              ColumnTitle := lColumn.Title.Caption;
              if (not AnsiSameText(ColumnTitle, Fields[I].FieldName))
                and (cbWithFieldName.Caption <> '') then
              begin
                if not cbWithFieldName.Visible then
                  cbWithFieldName.Show;
                if cbWithFieldName.State = cbChecked then
                  ColumnTitle := ColumnTitle + ' [' + Fields[I].FieldName + ']';
              end;
              J := clbList.Items.AddObject(ColumnTitle, TObject(lColumn.Index));
              clbList.Checked[J] := lColumn.Visible and Fields[I].Visible;
            end;
          end;
      end
      else
      begin
        if dgColumnResize in FJvDBGrid.Options then
        begin
          TListBox(clbList).ExtendedSelect := True;
          TListBox(clbList).MultiSelect := True;
        end;
        for I := 0 to Columns.Count - 1 do
        begin
          ColumnTitle := FJvDBGrid.Columns[I].Title.Caption;
          if not AnsiSameText(ColumnTitle, FJvDBGrid.Columns[I].FieldName)
            and (cbWithFieldName.Caption <> '') then
          begin
            if not cbWithFieldName.Visible then
              cbWithFieldName.Show;
            if cbWithFieldName.State = cbChecked then
              ColumnTitle := ColumnTitle + ' [' + FJvDBGrid.Columns[I].FieldName + ']';
          end;
          J := clbList.Items.AddObject(ColumnTitle, TObject(I));
          clbList.Checked[J] := FJvDBGrid.Columns[I].Visible;
        end;
      end;
      if clbList.Items.Count > 0 then
      begin
        clbList.ItemIndex := 0;
        clbList.Selected[0] := True;
      end;
      clbList.SetFocus;
    end;
  ResizeForm;
end;

procedure TfrmSelectColumn.cbClick(Sender: TObject);
begin
  FormActivate(Self);
end;

function TfrmSelectColumn.GetColumn(AField: TField): TColumn;
var
  I: Integer;
begin
  Result := nil;
  with FJvDBGrid.Columns do
    for I := 0 to Count - 1 do
    begin
      if Items[I].FieldName = AField.FieldName then
      begin
        Result := Items[I];
        Break;
      end;
    end;
end;

procedure TfrmSelectColumn.clbListClickCheck(Sender: TObject);
var
  I: Integer;
begin
  FCanHide := clbList.Items.Count = 0;
  if not FCanHide then
    for I := 0 to clbList.Items.Count - 1 do
    begin
      if clbList.Checked[I] then
      begin
        FCanHide := True;
        Break;
      end;
    end;
  if not FCanHide then
  begin
    MessageDlg(NoSelectionWarning, mtWarning, [mbOk], 0);
    if clbList.ItemIndex >= 0 then
    begin
      clbList.Checked[clbList.ItemIndex] := True;
      FCanHide := True;
    end;
  end;
end;

procedure TfrmSelectColumn.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Esc then
  begin
    ModalResult := mrCancel;
    FColumnUpdate := False;
  end;
end;

procedure TfrmSelectColumn.ResizeForm;
var
  MinHeight: Integer;
begin
  // Restrict Listbox to min 6 items, max 30 items and to screen height
  MinHeight := clbList.ItemHeight * 6;
  Constraints.MinHeight := Height - clbList.ClientHeight + MinHeight;
  MinHeight := clbList.ItemHeight * clbList.Items.Count;
  if MinHeight >= clbList.ItemHeight * 30 then MinHeight := clbList.ItemHeight * 30;
  MinHeight := Height - clbList.ClientHeight + MinHeight;
  if (Top + MinHeight) > Screen.MonitorFromRect(BoundsRect).Height then
    Top := Screen.MonitorFromRect(BoundsRect).Height - MinHeight;
  Height := MinHeight;
end;

procedure TfrmSelectColumn.FormResize(Sender: TObject);
var
  NewHeight: Integer;
begin
  // Set height so that allways full lines are visible
  NewHeight := clbList.ClientHeight div clbList.ItemHeight * clbList.ItemHeight;
  Height := Height - clbList.ClientHeight + NewHeight;
end;

initialization
  GetCheckSize;

{$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
