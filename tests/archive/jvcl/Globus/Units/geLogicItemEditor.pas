unit geLogicItemEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, glLogics,
  StdCtrls, ExtCtrls, Buttons, Menus, ComCtrls, ToolWin;

type
  TfLogicItemEditor = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    LB: TListBox;
    pRule_: TPanel;
    spRule: TSpeedButton;
    pValue: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    eValue: TEdit;
    pTrue: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    mTrue: TEdit;
    pFalse: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    mFalse: TEdit;
    Panel14: TPanel;
    Panel5: TPanel;
    pmRule: TPopupMenu;
    Panel18: TPanel;
    pExpr: TPanel;
    cbExpr: TComboBox;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Label1: TLabel;
    Label4: TLabel;
    pRule: TPanel;
    cbRule: TComboBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Label5: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    TB: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    Shape4: TShape;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LBMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure LBDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormShow(Sender: TObject);
    procedure spRuleClick(Sender: TObject);
    procedure cbExprChange(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure mTrueChange(Sender: TObject);
  private
    Logics: TLogics;
    LogicElement: TLogicElement;
    StopIndex: integer;
  public
    function Execute(Logics: TLogics; LogicElement: TLogicElement): boolean;
  end;

var
  fLogicItemEditor: TfLogicItemEditor;

implementation

{$R *.DFM}

{ TfLogicItemEditor }

function TfLogicItemEditor.Execute(Logics: TLogics; LogicElement: TLogicElement): boolean;
var i:integer;
begin
  Caption := 'Логический элемент :  ' + LogicElement.Caption;
  self.Logics := Logics;
  self.LogicElement := LogicElement;

  StopIndex := 10000;

  LB.Items.Clear;

  for i := 0 to LogicElement.LogicVariants.Count-1 do
    LB.Items.Add('1');

  cbExpr.Items.Clear;
  cbExpr.Items.Add('[RESULT]');
  for i:=0 to Logics.Dictionary.Count-1 do
    cbExpr.Items.Add('[' + Logics.Dictionary.Names[i] + ']');

  cbExpr.Text := LogicElement.Expression;
  cbRule.ItemIndex := integer(LogicElement.Rule);
  eValue.Text := LogicElement.Value;
  mTrue.Text := LogicElement.TrueResult;
  mFalse.Text := LogicElement.FalseResult;

  Result := ShowModal = mrOK;

  if not Result then exit;

  LogicElement.Expression := cbExpr.Text;
  LogicElement.Rule := TLogicRule(cbRule.ItemIndex);
  LogicElement.Value := eValue.Text;
  LogicElement.TrueResult := mTrue.Text;
  LogicElement.FalseResult := mFalse.Text;
end;

procedure TfLogicItemEditor.Button1Click(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfLogicItemEditor.Button2Click(Sender: TObject);
begin
  ModalResult := mrCANCEL;
end;

procedure TfLogicItemEditor.FormCreate(Sender: TObject);
var
  i: TLogicRule;
  j: integer;
  Item: TMenuItem;
begin
  for i:=low(TLogicRule) to high(TLogicRule) do
  begin
    cbRule.items.Add(LogicRuleLabels[i]);
    Item := NewItem(LogicRuleLabels[i], 0, false, true, nil, 255, '');
    pmRule.Items.Add(Item);
  end;
  TB.Color := $E0E0E0;
end;

procedure TfLogicItemEditor.LBMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  Height := 100;
end;

procedure TfLogicItemEditor.LBDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  R: TRect;
  Expression: string;
  Value: string;
  TrueResult: string;
  FalseResult: string;
begin
  with LB.Canvas do
  begin
    if odSelected in State then Brush.Color := $E0E0E0 else Brush.Color := clWindow;

    Expression := cbExpr.Text;
    Value := LogicElement.LogicVariants[Index].Value;
    TrueResult := LogicElement.LogicVariants[Index].TrueResult;
    FalseResult := LogicElement.LogicVariants[Index].FalseResult;

    if (TrueResult <> '')and(FalseResult <> '')then StopIndex := Index;


    FillRect(Rect);
    SetBkMode(Handle, TRANSPARENT);

    if StopIndex < Index then Font.Color := clSilver else Font.Color := clGray;
    R := Bounds(Rect.Left + 5, Rect.Top + 3, 120, 20);
    DrawText(Handle, 'если', 4, R, DT_SINGLELINE);

    Font.Color := $00007171;
    R := Bounds(Rect.Left + 130, Rect.Top + 3, 120, 20);
    DrawText(Handle, PChar(Expression), length(Expression), R, DT_SINGLELINE);


    if StopIndex < Index then Font.Color := clSilver else Font.Color := clGray;
    R := Bounds(Rect.Left + 20, Rect.Top + 20, 120, 20);
    DrawText(Handle, PChar(LogicRuleLabels[TLogicRule(cbRule.ItemIndex)]), length(LogicRuleLabels[TLogicRule(cbRule.ItemIndex)]), R, DT_SINGLELINE);

    if TLogicRule(cbRule.ItemIndex) <> ltNotEmpty then
    begin
      Font.Color := $00804000;
      R := Bounds(Rect.Left + 130, Rect.Top + 20, 120, 20);
      DrawText(Handle, PChar(Value), length(Value), R, DT_SINGLELINE);
    end;

    if StopIndex < Index then Font.Color := clSilver else Font.Color := clGray;
    R := Bounds(Rect.Left + 15, Rect.Top + 37, 120, 20);
    DrawText(Handle, 'то', 2, R, DT_SINGLELINE);

    Font.Color := clGreen;
    R := Bounds(Rect.Left + 130, Rect.Top + 37, 120, 20);
    DrawText(Handle, PChar(TrueResult), length(TrueResult), R, DT_SINGLELINE);


    Font.Color := clGray;
    R := Bounds(Rect.Left + 15, Rect.Top + 53, 120, 20);
    DrawText(Handle, 'иначе', 5, R, DT_SINGLELINE);

    Font.Color := $00404080;
    R := Bounds(Rect.Left + 130, Rect.Top + 53, 120, 20);
    DrawText(Handle, PChar(FalseResult), length(FalseResult), R, DT_SINGLELINE);


    Brush.Color := clGray;
    R := Classes.Rect(Rect.Left + 10, Rect.Bottom-1, Rect.Right-10, Rect.Bottom);
    FillRect(R);

    if odSelected in State then
    begin
     eValue.Text := LogicElement.LogicVariants[LB.ItemIndex].Value;
     mTrue.Text := LogicElement.LogicVariants[LB.ItemIndex].TrueResult;
     mFalse.Text := LogicElement.LogicVariants[LB.ItemIndex].FalseResult;
//      pExpr.Left := LB.Left + 100;
//      pExpr.Top := LB.Top + Rect.Top + 2;
      eValue.Text := Value;
      mTrue.Text := TrueResult;
      mFalse.Text := FalseResult;

      pRule_.Left := LB.Left + 3;
      pRule_.Top := LB.Top + Rect.Top+22;

      pValue.Left := LB.Left + 130;
      pValue.Top := LB.Top + Rect.Top+19;
      pValue.Width := 290;
      pValue.Visible := TLogicRule(cbRule.ItemIndex) <> ltNotEmpty;

      pTrue.Left := LB.Left + 130;
      pTrue.Top := LB.Top + Rect.Top+36;
      pTrue.Width := 290;

      pFalse.Left := LB.Left + 130;
      pFalse.Top := LB.Top + Rect.Top+53;
      pFalse.Width := 290;

      DrawFocusRect(Rect);
    end;
  end;
end;

procedure TfLogicItemEditor.FormShow(Sender: TObject);
begin
  LB.ItemIndex := 0;
end;

procedure TfLogicItemEditor.spRuleClick(Sender: TObject);
var pt: TPoint;
begin
  pt.X := spRule.Left;
  pt.Y := spRule.Top + spRule.Height;
  pt := spRule.ClientToScreen(pt);
  pmRule.Popup(pt.X, pt.Y);
end;

procedure TfLogicItemEditor.cbExprChange(Sender: TObject);
begin
  LB.Invalidate;
end;

procedure TfLogicItemEditor.ToolButton1Click(Sender: TObject);
begin
  with LogicElement.LogicVariants.Add do
  begin
    Value := '[ не определено]';
    TrueResult := '[ не определено ]';
    FalseResult := '[ не определено ]';
  end;
  LB.Items.Add('1');
end;

procedure TfLogicItemEditor.mTrueChange(Sender: TObject);
begin
  if LB.ItemIndex = -1 then exit;
  LogicElement.LogicVariants[LB.ItemIndex].Value := eValue.Text;
  LogicElement.LogicVariants[LB.ItemIndex].TrueResult := mTrue.Text;
  LogicElement.LogicVariants[LB.ItemIndex].FalseResult := mFalse.Text;
end;

end.
