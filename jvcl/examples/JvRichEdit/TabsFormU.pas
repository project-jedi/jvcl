{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit TabsFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ActnList, JvMaskEdit, JvSpin, JvRichEdit,
  Contnrs;

type
  TMeasurementUnit = (mtInches, mtCentimeters, mtMillimeters, mtPoints, mtPicas);

  TTabsForm = class(TForm)
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    edtTabStopPosition: TEdit;
    lsbTabStopPositions: TListBox;
    pnlLeader: TPanel;
    pnlAlignment: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    StaticText1: TStaticText;
    StaticText3: TStaticText;
    ActionList1: TActionList;
    actSet: TAction;
    actClear: TAction;
    actClearAll: TAction;
    actOK: TAction;
    actCancel: TAction;
    Bevel3: TBevel;
    Bevel2: TBevel;
    Bevel1: TBevel;
    rbtAlignment1: TRadioButton;
    rbtAlignment2: TRadioButton;
    rbtAlignment5: TRadioButton;
    rbtAlignment3: TRadioButton;
    rbtAlignment4: TRadioButton;
    rbtLeader1: TRadioButton;
    rbtLeader2: TRadioButton;
    rbtLeader3: TRadioButton;
    rbtLeader4: TRadioButton;
    cmbMeasurementUnits: TComboBox;
    rbtLeader5: TRadioButton;
    procedure actSetExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actClearAllExecute(Sender: TObject);
    procedure actOKExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure edtTabStopPositionKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lsbTabStopPositionsEnter(Sender: TObject);
    procedure lsbTabStopPositionsClick(Sender: TObject);
    procedure cmbMeasurementUnitsChange(Sender: TObject);
    procedure AlignmentClick(Sender: TObject);
    procedure IsTabStopFilled(Sender: TObject);
    procedure lsbTabStopPositionsDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
  private
    FTabs: TObjectList;
    FLeaderObjs: array [TJvTabLeader] of TRadioButton;
    FAlignmentObjs: array [TJvTabAlignment] of TRadioButton;
    procedure SetTabAlignment(const Value: TJvTabAlignment);
    procedure SetTabLeader(const Value: TJvTabLeader);
    function GetTabAlignment: TJvTabAlignment;
    function GetTabLeader: TJvTabLeader;
  private
    FMeasurementUnit: TMeasurementUnit;
    procedure Init;
  protected
    function FindTab(const APosition: Integer; var Index: Integer): Boolean;
    procedure ClearAllTabStops;
    procedure ClearTabStop;
    procedure FillMeasurementUnits(Strings: TStrings);
    procedure GetTabs(Paragraph: TJvParaAttributes);
    procedure GotoTab(const Index: Integer);
    procedure SelectTab(const Index: Integer);
    procedure SetTabs(Paragraph: TJvParaAttributes);
    procedure SetTabStop;
    procedure UpdateMeasurementUnits;
    procedure UpdateLeader;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property TabAlignment: TJvTabAlignment read GetTabAlignment write SetTabAlignment;
    property TabLeader: TJvTabLeader read GetTabLeader write SetTabLeader;
  end;

function FormatTabs(Paragraph: TJvParaAttributes): Boolean;

implementation

uses
  Math;

{$R *.dfm}

function TryStrToFloat(const S:string; Var Value:Extended):boolean;
begin
  try
    Result := true;
    Value := StrToFloat(S);
  except
    Result := false;
  end;
end;

type
  TTabObj = class(TObject)
  private
    FPosition: Longint;
    FAlignment: TJvTabAlignment;
    FLeader: TJvTabLeader;
  public
    constructor Create(const APosition: Longint; const AAlignment: TJvTabAlignment;
      const ALeader: TJvTabLeader);
    { Tab position in points }
    property Position: Longint read FPosition write FPosition;
    property Alignment: TJvTabAlignment read FAlignment write FAlignment;
    property Leader: TJvTabLeader read FLeader write fLeader;
  end;

const
  CInitialMeasurementUnit = mtCentimeters;

  CPointsPerInch = 72;
  CPointsPerMM = 2.83464567;
  CPointsPerPica = 12;

  CMeasurementUnitStr: array [TMeasurementUnit] of string =
    ('in', 'cm', 'mm', 'pt', 'pi');
  CMeasurementUnitLongStr: array [TMeasurementUnit] of string =
    ('Inches', 'Centimeters', 'Millimeters', 'Points', 'Picas');

function MeasurementUnitToPoints(const Value: Extended; const AMeasurementUnit: TMeasurementUnit): Integer;
begin
  case AMeasurementUnit of
    mtInches: Result := Round(Value * CPointsPerInch);
    mtCentimeters: Result := Round(Value * CPointsPerMM * 10);
    mtMillimeters: Result := Round(Value * CPointsPerMM);
    mtPoints: Result := Round(Value);
    mtPicas: Result := Round(Value * CPointsPerPica);
  else
    raise Exception.Create('Invalid measurement unit');
  end;
end;

function PointsToMeasurementUnit(const Value: Integer; const AMeasurementUnit: TMeasurementUnit): Extended;
begin
  case AMeasurementUnit of
    mtInches: Result := Value / CPointsPerInch;
    mtCentimeters: Result := (Value / CPointsPerMM) / 10;
    mtMillimeters: Result := Value / CPointsPerMM;
    mtPoints: Result := Value;
    mtPicas: Result := Value / CPointsPerPica;
  else
    raise Exception.Create('Invalid measurement unit');
  end;
end;

function StrToMeasurementUnit(const S: string; var MeasurementUnit: TMeasurementUnit): Boolean;
var
  LMeasurementUnit: TMeasurementUnit;
begin
  for LMeasurementUnit := Low(TMeasurementUnit) to High(TMeasurementUnit) do
    if SameText(S, CMeasurementUnitStr[LMeasurementUnit]) then
    begin
      Result := True;
      MeasurementUnit := LMeasurementUnit;
      Exit;
    end;
  Result := False;
end;

function TextToPosition(S: string; const Default: TMeasurementUnit): Integer;
var
  MeasurementUnit: TMeasurementUnit;
  ValueStr: string;
  Value: Extended;
begin
  S := Trim(S);
  if StrToMeasurementUnit(Copy(S, Length(S) - 1, 2), MeasurementUnit) then
    ValueStr := Trim(Copy(S, 1, Length(S) - 2))
  else
  begin
    ValueStr := S;
    MeasurementUnit := Default;
  end;
  if TryStrToFloat(ValueStr, Value) then
    Result := MeasurementUnitToPoints(Value, MeasurementUnit)
  else
    raise Exception.Create('This is not a valid tab stop.');
end;

function DigitsNeeded(Value: Extended): Integer;
const
  CMaxDigits = 2;
  CEpsilons: array [0..CMaxDigits - 1] of Extended = (0.05, 0.005);
var
  Diff: Extended;
begin
  Result := 0;
  while Result < CMaxDigits do
  begin
    Diff := Value - Round(Value);
    if Diff < 0 then
      Diff := -Diff;
    if Diff < CEpsilons[CMaxDigits - Result - 1] then
      Exit;
    Value := Value * 10;
    Inc(Result);
  end;
end;

function PositionToText(const APosition: Integer; const MeasurementUnit: TMeasurementUnit): string;
var
  Value: Extended;
begin
  Value := PointsToMeasurementUnit(APosition, MeasurementUnit);
  Result := Format('%.*f %s', [DigitsNeeded(Value), Value,
    CMeasurementUnitStr[MeasurementUnit]]);
end;

function FormatTabs(Paragraph: TJvParaAttributes): Boolean;
begin
  with TTabsForm.Create(Application) do
  try
    SetTabs(Paragraph);
    Result := ShowModal = mrOk;
    if Result then
      GetTabs(Paragraph);
  finally
    Free;
  end;
end;

//=== TTabObj ================================================================

constructor TTabObj.Create(const APosition: Integer;
  const AAlignment: TJvTabAlignment; const ALeader: TJvTabLeader);
begin
  inherited Create;
  FPosition := APosition;
  FAlignment := AAlignment;
  FLeader := ALeader;
end;

//=== TTabsForm ==============================================================

procedure TTabsForm.actCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TTabsForm.actClearAllExecute(Sender: TObject);
begin
  ClearAllTabStops;
end;

procedure TTabsForm.actClearExecute(Sender: TObject);
begin
  ClearTabStop;
end;

procedure TTabsForm.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TTabsForm.actSetExecute(Sender: TObject);
begin
  SetTabStop;
end;

procedure TTabsForm.AlignmentClick(Sender: TObject);
begin
  UpdateLeader;
end;

procedure TTabsForm.ClearAllTabStops;
begin
  FTabs.Clear;
  lsbTabStopPositions.Items.Clear;
//  lsbTabStopPositions.Count := FTabs.Count;
  GotoTab(-1);
end;

procedure TTabsForm.ClearTabStop;
var
  Index: Integer;
begin
  Index := lsbTabStopPositions.ItemIndex;
  if (Index >= 0) and (Index < FTabs.Count) then
  begin
    FTabs.Delete(Index);
    lsbTabStopPositions.Items.Delete(Index);
  end;
  GotoTab(Min(Index, FTabs.Count - 1));
end;

procedure TTabsForm.cmbMeasurementUnitsChange(Sender: TObject);
begin
  UpdateMeasurementUnits;
end;

constructor TTabsForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTabs := TObjectList.Create;
  Init;
end;

destructor TTabsForm.Destroy;
begin
  FTabs.Free;
  inherited Destroy;
end;

procedure TTabsForm.edtTabStopPositionKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DOWN) and not (ssAlt in Shift) then
  begin
    GotoTab(Min(lsbTabStopPositions.ItemIndex + 1, FTabs.Count - 1));
    Key := 0;
  end
  else
  if (Key = VK_UP) and not (ssAlt in Shift) then
  begin
    GotoTab(Max(lsbTabStopPositions.ItemIndex - 1, 0));
    Key := 0;
  end
  else
  if Key = VK_RETURN then
    SetTabStop;
end;

procedure TTabsForm.FillMeasurementUnits(Strings: TStrings);
var
  MeasurementUnit: TMeasurementUnit;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for MeasurementUnit := Low(TMeasurementUnit) to High(TMeasurementUnit) do
      Strings.AddObject(CMeasurementUnitLongStr[MeasurementUnit], TObject(MeasurementUnit));
  finally
    Strings.EndUpdate;
  end;
end;

function TTabsForm.FindTab(const APosition: Integer;
  var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FTabs.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := TTabObj(FTabs[I]).Position - APosition;
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        Index := I;
        Exit;
      end;
    end;
  end;
  Index := L;
end;

function TTabsForm.GetTabAlignment: TJvTabAlignment;
var
  Alignment: TJvTabAlignment;
begin
  for Alignment := Low(TJvTabAlignment) to High(TJvTabAlignment) do
    if Assigned(FAlignmentObjs[Alignment]) and
      FAlignmentObjs[Alignment].Checked then
    begin
      Result := Alignment;
      Exit;
    end;
  Result := Low(TJvTabAlignment);
end;

function TTabsForm.GetTabLeader: TJvTabLeader;
var
  Leader: TJvTabLeader;
begin
  for Leader := Low(TJvTabLeader) to High(TJvTabLeader) do
    if Assigned(FLeaderObjs[Leader]) and
      FLeaderObjs[Leader].Checked then
    begin
      Result := Leader;
      Exit;
    end;
  Result := Low(TJvTabLeader);
end;

procedure TTabsForm.GetTabs(Paragraph: TJvParaAttributes);
var
  I: Integer;
begin
  Paragraph.TabCount := FTabs.Count;

  for I := 0 to FTabs.Count - 1 do
    with TTabObj(FTabs[i]) do
    begin
      Paragraph.Tab[i] := Position;
      Paragraph.TabAlignment[i] := Alignment;
      Paragraph.TabLeader[i] := Leader;
    end;
end;

procedure TTabsForm.GotoTab(const Index: Integer);
begin
  lsbTabStopPositions.ItemIndex := Index;
  SelectTab(Index);
end;

procedure TTabsForm.Init;
var
  Leader: TJvTabLeader;
  Alignment: TJvTabAlignment;
begin
  FillChar(FLeaderObjs, SizeOf(FLeaderObjs), #0);
  FillChar(FAlignmentObjs, SizeOf(FAlignmentObjs), #0);

  FLeaderObjs[tlNone] := rbtLeader1;
  FLeaderObjs[tlDotted] := rbtLeader2;
  FLeaderObjs[tlDashed] := rbtLeader3;
  FLeaderObjs[tlUnderlined] := rbtLeader4;
  FLeaderObjs[tlDouble] := rbtLeader5;

  FAlignmentObjs[taOrdinary] := rbtAlignment1;
  FAlignmentObjs[taCenter] := rbtAlignment2;
  FAlignmentObjs[taRight] := rbtAlignment3;
  FAlignmentObjs[taDecimal] := rbtAlignment4;
  FAlignmentObjs[taVertical] := rbtAlignment5;

  if RichEditVersion < 3 then
  begin
    for Leader := Low(TJvTabLeader) to High(TJvTabLeader) do
      if (Leader <> tlNone) and Assigned(FLeaderObjs[Leader]) then
        FLeaderObjs[Leader].Enabled := False;
    for Alignment := Low(TJvTabAlignment) to High(TJvTabAlignment) do
      if (Alignment <> taOrdinary) and Assigned(FAlignmentObjs[Alignment]) then
        FAlignmentObjs[Alignment].Enabled := False;
  end;

  FillMeasurementUnits(cmbMeasurementUnits.Items);
  FMeasurementUnit := CInitialMeasurementUnit;
  with cmbMeasurementUnits do
    ItemIndex := Items.IndexOfObject(TObject(FMeasurementUnit));
end;

procedure TTabsForm.IsTabStopFilled(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := edtTabStopPosition.Text > '';
end;

procedure TTabsForm.lsbTabStopPositionsClick(Sender: TObject);
begin
  SelectTab(lsbTabStopPositions.ItemIndex);
end;

procedure TTabsForm.lsbTabStopPositionsEnter(Sender: TObject);
begin
  edtTabStopPosition.SetFocus;
end;

procedure TTabsForm.SelectTab(const Index: Integer);
begin
  if (Index < 0) or (Index >= FTabs.Count) then
  begin
    edtTabStopPosition.Text := '';
    if ActiveControl = lsbTabStopPositions then
      edtTabStopPosition.SetFocus;
    Exit;
  end;

  with TTabObj(FTabs[Index]) do
  begin
    edtTabStopPosition.Text := PositionToText(Position, FMeasurementUnit);
    TabLeader := Leader;
    TabAlignment := Alignment;
  end;

  if Self.Visible then
    with edtTabStopPosition do
    begin
      if Self.ActiveControl = lsbTabStopPositions then
        SetFocus;
      SelectAll;
    end;
end;

procedure TTabsForm.SetTabAlignment(const Value: TJvTabAlignment);
begin
  if Assigned(FAlignmentObjs[Value]) then
    FAlignmentObjs[Value].Checked := True;
end;

procedure TTabsForm.SetTabLeader(const Value: TJvTabLeader);
begin
  if Assigned(FLeaderObjs[Value]) then
    FLeaderObjs[Value].Checked := True;
end;

procedure TTabsForm.SetTabs(Paragraph: TJvParaAttributes);
var
  I: Integer;
begin
  for I := 0 to Paragraph.TabCount - 1 do
  begin
    FTabs.Add(TTabObj.Create(Paragraph.Tab[i], Paragraph.TabAlignment[i], Paragraph.TabLeader[i]));
    lsbTabStopPositions.Items.Add('');
  end;
  if lsbTabStopPositions.Items.Count > 0 then
  begin
    lsbTabStopPositions.ItemIndex := 0;
    SelectTab(0);
  end
  else
  begin
    edtTabStopPosition.Text := '';
    rbtLeader1.Checked := True;
    rbtAlignment1.Checked := True;
  end;
end;

procedure TTabsForm.SetTabStop;
var
  Position: Integer;
  Index: Integer;
begin
  Position := TextToPosition(edtTabStopPosition.Text, FMeasurementUnit);
  if FindTab(Position, Index) then
    with TTabObj(FTabs[Index]) do
    begin
      Alignment := TabAlignment;
      Leader := TabLeader;
    end
  else
  begin
    FTabs.Insert(Index, TTabObj.Create(Position, TabAlignment, TabLeader));
    lsbTabStopPositions.Items.Add('');
  end;
  GotoTab(Index);
end;

procedure TTabsForm.UpdateLeader;
var
  Leader: TJvTabLeader;
begin
  if not Assigned(FAlignmentObjs[taVertical]) then
    Exit;

  for Leader := Low(TJvTabLeader) to High(TJvTabLeader) do
    if Assigned(FLeaderObjs[Leader]) then
      FLeaderObjs[Leader].Enabled :=
        (RichEditVersion >= 3) and not FAlignmentObjs[taVertical].Checked;
end;

procedure TTabsForm.UpdateMeasurementUnits;
var
  Index: Integer;
begin
  Index := cmbMeasurementUnits.ItemIndex;
  if Index < 0 then
    Exit;
  Index := Integer(cmbMeasurementUnits.Items.Objects[Index]);
  if (Index < 0) or (Index > Integer(High(TMeasurementUnit))) then
    Exit;
  FMeasurementUnit := TMeasurementUnit(Index);
  lsbTabStopPositions.Refresh;
  GotoTab(lsbTabStopPositions.ItemIndex);
end;

procedure TTabsForm.lsbTabStopPositionsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var S:String;
begin
  if (Index >= 0) and (Index < FTabs.Count) then
  begin
    S := PositionToText(TTabObj(FTabs[Index]).Position, FMeasurementUnit);
    lsbTabStopPositions.Canvas.FillRect(Rect);
    lsbTabStopPositions.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, S);
  end;


end;

end.

