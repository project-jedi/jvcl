{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBLookupEdit.PAS, released on 2003-09-18.
The Code was modified to: JvDBLookupComboEdit.PAS, released on 2003-10-20.

The Initial Developers of the Original Code are: Michael Habbe
Copyright (c) 2003 Michael Habbe
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
=============

It must inherit from JvLookupEdit, because the new structure in JVCL3 produced
(in my environment) stack-overflow-errors, which "kicked" Delphi out of the
memory with only the message "Danger. Stack-Overflow. Save your work and restart
Delphi.". (The message is in German and i never saw it before!?)

I find out the problem in line 286 "or inherited ReadOnly;", when i uncommented
it, Delphi works, but i can modify the dataset, although i set ReadOnly to True.

As aforesaid, the component works in my Delphi with JVCL2, but as soon as i
inherit it with JVCL3 from JvDBLookupEdit, the specified errors occur.

Michael Habbe [2003-10-20]
-----------------------------------------------------------------------------}
// $Id$

unit JvDBLookupComboEdit;

{$I jvcl.inc}

interface

uses
  Windows,
  {$IFDEF VCL}
  Messages,
  {$ENDIF VCL}
  Classes, Controls, Graphics, DB, DBCtrls,
  JvDBLookup;

type
  TJvDBLookupComboEdit = class(TJvDBLookupEdit)
  private
    FDataLink: TFieldDataLink;
    FCanvas: TControlCanvas;
    // FAlignment: TAlignment;
    FFocused: Boolean;
    FBeepOnError: Boolean;
    procedure ActiveChange(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetCanvas: TCanvas;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetTextMargins: TPoint;
    procedure ResetMaxLength;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
  protected
    procedure DoClipboardCut; override;
    procedure DoClipboardPaste; override;
    procedure DoUndo; override;
    procedure DoEnter; override;
    procedure DoExit; override;

    function GetReadOnly: Boolean; override; // suppress the warning
    procedure SetReadOnly(Value: Boolean); override;
    procedure Change; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Reset; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    // function UseRightToLeftAlignment: Boolean; override;
    property Field: TField read GetField;
    property Canvas: TCanvas read GetCanvas;
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default True;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  SysUtils, Forms, StdCtrls,
  JvConsts;

constructor TJvDBLookupComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // inherited ReadOnly := True;
  ControlStyle := ControlStyle + [csReplicatable];
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnActiveChange := ActiveChange;
  FBeepOnError := True;
end;

destructor TJvDBLookupComboEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
  // (rom) destroy Canvas AFTER inherited Destroy
  FCanvas.Free;
end;

procedure TJvDBLookupComboEdit.ResetMaxLength;
var
  F: TField;
begin
  if (MaxLength > 0) and Assigned(DataSource) and Assigned(DataSource.DataSet) then
  begin
    F := DataSource.DataSet.FindField(DataField);
    if Assigned(F) and (F.DataType in [ftString, ftWideString]) and (F.Size = MaxLength) then
      MaxLength := 0;
  end;
end;

procedure TJvDBLookupComboEdit.Loaded;
begin
  inherited Loaded;
  ResetMaxLength;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

procedure TJvDBLookupComboEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;

//function TJvDBLookupComboEdit.UseRightToLeftAlignment: Boolean;
//begin
//  Result := DBUseRightToLeftAlignment(Self, Field);
//end;

procedure TJvDBLookupComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
//  inherited KeyDown(Key, Shift);
//  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
//    FDataLink.Edit;

  // new order, because result of inherited KeyDown(...) could be = 0
  // so, first set DataSet in Edit-Mode
  if Key in [VK_BACK, VK_DELETE, VK_UP, VK_DOWN, 32..255] then // taken from TDBComboBox.KeyDown(...)
    FDataLink.Edit;
  inherited KeyDown(Key, Shift);
end;

procedure TJvDBLookupComboEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    if BeepOnError then
      SysUtils.Beep;
    Key := #0;
  end;
  case Key of
    CtrlH, CtrlV, CtrlX, #32..#255:
      FDataLink.Edit;
    Esc:
      begin
        FDataLink.Reset;
        SelectAll;
//        Key := #0;
      end;
  end;
end;

function TJvDBLookupComboEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

procedure TJvDBLookupComboEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TJvDBLookupComboEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    // if (FAlignment <> taLeftJustify) and not IsMasked then Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TJvDBLookupComboEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

function TJvDBLookupComboEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBLookupComboEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

function TJvDBLookupComboEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBLookupComboEdit.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then
    ResetMaxLength;
  FDataLink.FieldName := Value;
end;

function TJvDBLookupComboEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly or inherited GetReadOnly;
end;

procedure TJvDBLookupComboEdit.SetReadOnly(Value: Boolean);
begin
  inherited SetReadOnly(Value);
  FDataLink.ReadOnly := Value;
end;

function TJvDBLookupComboEdit.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TJvDBLookupComboEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TJvDBLookupComboEdit.ActiveChange(Sender: TObject);
begin
  ResetMaxLength;
end;

procedure TJvDBLookupComboEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    //if FAlignment <> FDataLink.Field.Alignment then
    //begin
    //  EditText := '';  {forces update}
    //  FAlignment := FDataLink.Field.Alignment;
    //end;
    EditMask := FDataLink.Field.EditMask;
    if not (csDesigning in ComponentState) then
      if (FDataLink.Field.DataType in [ftString, ftWideString]) and (MaxLength = 0) then
        MaxLength := FDataLink.Field.Size;
    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
    begin
      EditText := FDataLink.Field.DisplayText;
      if FDataLink.Editing then //and FDataLink.FModified || fmodified is private in parent of fdatalink
        Modified := True;
    end;
  end
  else
  begin
    //FAlignment := taLeftJustify;
    EditMask := '';
    if csDesigning in ComponentState then
      EditText := Name
    else
      EditText := '';
  end;
end;

procedure TJvDBLookupComboEdit.EditingChange(Sender: TObject);
begin
  //ReadOnly := not FDataLink.Editing;
end;

procedure TJvDBLookupComboEdit.UpdateData(Sender: TObject);
begin
  ValidateEdit;
  FDataLink.Field.Text := Text;
end;

procedure TJvDBLookupComboEdit.DoUndo;
begin
  FDataLink.Edit;
  inherited DoUndo;
end;

procedure TJvDBLookupComboEdit.DoClipboardPaste;
begin
  FDataLink.Edit;
  inherited DoClipboardPaste;
end;

procedure TJvDBLookupComboEdit.DoClipboardCut;
begin
  FDataLink.Edit;
  inherited DoClipboardCut;
end;

procedure TJvDBLookupComboEdit.DoEnter;
begin
  SetFocused(True);
  inherited DoEnter;
  if SysLocale.FarEast and FDataLink.CanModify then
    inherited ReadOnly := False;
end;

procedure TJvDBLookupComboEdit.DoExit;
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;
  inherited DoExit;
end;

procedure TJvDBLookupComboEdit.WMPaint(var Msg: TWMPaint);
const
  AlignStyle: array [Boolean, TAlignment] of DWORD =
   ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
var
  Left: Integer;
  Margins: TPoint;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  AAlignment: TAlignment;
  ExStyle: DWORD;
begin
  if csDestroying in ComponentState then
    Exit;
  AAlignment := Alignment; //FAlignment;
  if UseRightToLeftAlignment then
    ChangeBiDiModeAlignment(AAlignment);
  if ((AAlignment = taLeftJustify) or FFocused) and
    not (csPaintCopy in ControlState) then
  begin
    if SysLocale.MiddleEast and HandleAllocated and (IsRightToLeft) then
    begin { This keeps the right aligned text, right aligned }
      ExStyle := DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) and (not WS_EX_RIGHT) and
        (not WS_EX_RTLREADING) and (not WS_EX_LEFTSCROLLBAR);
      if UseRightToLeftReading then
        ExStyle := ExStyle or WS_EX_RTLREADING;
      if UseRightToLeftScrollbar then
        ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
      ExStyle := ExStyle or
        AlignStyle[UseRightToLeftAlignment, AAlignment];
      if DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) <> ExStyle then
        SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
    end;
    inherited;
    Exit;
  end;

{ Since edit controls do not handle justification unless multi-line (and
  then only poorly) we will draw right and center justify manually unless
  the edit has the focus. }
  DC := Msg.DC;
  if DC = 0 then
    DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do
    begin
      R := ClientRect;
      if not (NewStyleControls and Ctl3D) and (BorderStyle = bsSingle) then
      begin
        Brush.Color := clWindowFrame;
        FrameRect(R);
        InflateRect(R, -1, -1);
      end;
      Brush.Color := Color;
      if not Enabled then
        Font.Color := clGrayText;
      if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
      begin
        S := FDataLink.Field.DisplayText;
        case CharCase of
          ecUpperCase:
            S := AnsiUpperCase(S);
          ecLowerCase:
            S := AnsiLowerCase(S);
        end;
      end
      else
        S := EditText;
      if PasswordChar <> #0 then
        FillChar(S[1], Length(S), PasswordChar);
      Margins := GetTextMargins;
      case AAlignment of
        taLeftJustify:
          Left := Margins.X;
        taRightJustify:
          Left := ClientWidth - TextWidth(S) - Margins.X - 1;
      else
        Left := (ClientWidth - TextWidth(S)) div 2;
      end;
      if SysLocale.MiddleEast then
        UpdateTextFlags;
      TextRect(R, Left, Margins.Y, S);
    end;
  finally
    FCanvas.Handle := 0;
    if Msg.DC = 0 then
      EndPaint(Handle, PS);
  end;
end;

procedure TJvDBLookupComboEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Integer(FDataLink);
end;

function TJvDBLookupComboEdit.GetTextMargins: TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  if NewStyleControls then
  begin
    if BorderStyle = bsNone then
      I := 0
    else
    if Ctl3D then
      I := 1
    else
      I := 2;
    Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
    Result.Y := I;
  end
  else
  begin
    if BorderStyle = bsNone then
      I := 0
    else
    begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then
        I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;

function TJvDBLookupComboEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TJvDBLookupComboEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

end.

