unit JvFullColorListFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ColorSpaces, FullColorFrm, StdCtrls, ColorDialogs, ActnList,
  Buttons, ImgList, ColorCtrls;

type
  TJvFullColorListForm = class(TForm)
    JvFullColorDialog: TJvFullColorDialog;
    ListBoxColors: TListBox;
    ActionList: TActionList;
    ActionNew: TAction;
    ActionModify: TAction;
    ActionDelete: TAction;
    ButtonNew: TButton;
    ButtonModify: TButton;
    ButtonDelete: TButton;
    Button4: TButton;
    ButtonOK: TButton;
    BitBtnMoveUp: TBitBtn;
    ActionMoveUp: TAction;
    ActionMoveDown: TAction;
    BitBtnMoveDown: TBitBtn;
    ButtonApply: TButton;
    Button1: TButton;
    ActionClear: TAction;
    Button2: TButton;
    ActionInsert: TAction;
    procedure ActionNewUpdate(Sender: TObject);
    procedure ActionModifyUpdate(Sender: TObject);
    procedure ActionDeleteUpdate(Sender: TObject);
    procedure ActionMoveUpUpdate(Sender: TObject);
    procedure ActionMoveDownUpdate(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionClearUpdate(Sender: TObject);
    procedure ActionModifyExecute(Sender: TObject);
    procedure ActionInsertUpdate(Sender: TObject);
    procedure ActionClearExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionInsertExecute(Sender: TObject);
    procedure ActionMoveUpExecute(Sender: TObject);
    procedure ActionMoveDownExecute(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure JvFullColorDialogApply(Sender: TObject;
      AFullColor: TJvFullColor);
    procedure ListBoxColorsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    FColorList:TJvFullColorList;
    FOnApply: TNotifyEvent;
    procedure SetColorList(const Value: TJvFullColorList);
    function GetColorList: TJvFullColorList;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    function Execute:Boolean;
    property ColorList:TJvFullColorList read GetColorList write SetColorList;
    property OnApply:TNotifyEvent read FOnApply write FOnApply;
  end;

var
  JvFullColorListForm: TJvFullColorListForm;

implementation

{$R *.dfm}

procedure TJvFullColorListForm.ActionClearExecute(Sender: TObject);
begin
  ListBoxColors.Clear;
end;

procedure TJvFullColorListForm.ActionClearUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=ListBoxColors.Count>0;
end;

procedure TJvFullColorListForm.ActionDeleteExecute(Sender: TObject);
begin
  ListBoxColors.DeleteSelected;
end;

procedure TJvFullColorListForm.ActionDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=ListBoxColors.SelCount>=1;
end;

procedure TJvFullColorListForm.ActionInsertExecute(Sender: TObject);
begin
  JvFullColorDialog.Options:=JvFullColorDialog.Options-[foShowApply];
  if (JvFullColorDialog.Execute) then
    ListBoxColors.Items.InsertObject(ListBoxColors.ItemIndex,'',
      TObject(JvFullColorDialog.FullColor));
end;

procedure TJvFullColorListForm.ActionInsertUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=ListBoxColors.SelCount=1;
end;

procedure TJvFullColorListForm.ActionModifyExecute(Sender: TObject);
begin
  JvFullColorDialog.Options:=JvFullColorDialog.Options+[foShowApply];
  JvFullColorDialog.FullColor:=TJvFullColor(ListBoxColors.Items.Objects[ListBoxColors.ItemIndex]);
  if (JvFullColorDialog.Execute) then
    ListBoxColors.Items.Objects[ListBoxColors.ItemIndex]:=TObject(JvFullColorDialog.FullColor);
end;

procedure TJvFullColorListForm.ActionModifyUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=ListBoxColors.SelCount=1;
end;

procedure TJvFullColorListForm.ActionMoveDownExecute(Sender: TObject);
var
  OldIndex:Integer;
begin
  with ListBoxColors do
  begin
    OldIndex:=ItemIndex;
    Items.Move(ItemIndex,ItemIndex+1);
    Selected[OldIndex+1]:=True;
  end;
end;

procedure TJvFullColorListForm.ActionMoveDownUpdate(Sender: TObject);
begin
  with ListBoxColors do
    (Sender as TAction).Enabled:=(SelCount=1) and (ItemIndex<(Count-1));
end;

procedure TJvFullColorListForm.ActionMoveUpExecute(Sender: TObject);
var
  OldIndex:Integer;
begin
  with ListBoxColors do
  begin
    OldIndex:=ItemIndex;
    Items.Move(ItemIndex,ItemIndex-1);
    Selected[OldIndex-1]:=True;
  end;
end;

procedure TJvFullColorListForm.ActionMoveUpUpdate(Sender: TObject);
begin
  with ListBoxColors do
    (Sender as TAction).Enabled:=(SelCount=1) and (ItemIndex>0);
end;

procedure TJvFullColorListForm.ActionNewExecute(Sender: TObject);
begin
  JvFullColorDialog.Options:=JvFullColorDialog.Options-[foShowApply];
  if (JvFullColorDialog.Execute) then
    ListBoxColors.Items.AddObject('',TObject(JvFullColorDialog.FullColor));
end;

procedure TJvFullColorListForm.ActionNewUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=True;
end;

constructor TJvFullColorListForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorList:=TJvFullColorList.Create;
end;

destructor TJvFullColorListForm.Destroy;
begin
  FColorList.Free;
  inherited Destroy;
end;

function TJvFullColorListForm.Execute: Boolean;
begin
  Result:=(ShowModal = mrOK);
end;

function TJvFullColorListForm.GetColorList: TJvFullColorList;
var
  Index:Integer;
begin
  FColorList.BeginUpdate;
  FColorList.Clear;
  For Index:=0 to ListBoxColors.Count-1 do
    FColorList.Add(TJvFullColor(ListBoxColors.Items.Objects[Index]));
  FColorList.EndUpdate;
  Result:=FColorList;
end;

procedure TJvFullColorListForm.SetColorList(const Value: TJvFullColorList);
var
  Index:Integer;
begin
  with ListBoxColors.Items, ColorSpaceManager do
  begin
    BeginUpdate;
    for Index:=0 to Value.Count-1 do
      AddObject('',TObject(Value.Items[Index]));
    EndUpdate;
  end;
end;

procedure TJvFullColorListForm.ButtonApplyClick(Sender: TObject);
begin
  if Assigned(FOnApply)
    then FOnApply(Self);
end;

procedure TJvFullColorListForm.JvFullColorDialogApply(Sender: TObject;
  AFullColor: TJvFullColor);
begin
  ListBoxColors.Items.Objects[ListBoxColors.ItemIndex]:=TObject(JvFullColorDialog.FullColor);
end;

procedure TJvFullColorListForm.ListBoxColorsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  AFullColor:TJvFullColor;
begin
  with TListBox(Control), Canvas do
  begin
    if (odSelected in State) then
      Font.Color:=clCaptionText;

    Pen.Style:=psSolid;
    Pen.Color:=Brush.Color;
    Brush.Style:=bsSolid;

    Rectangle(Rect);

    AFullColor:=TJvFullColor(Items.Objects[Index]);

    with ColorSpaceManager, ColorSpace[GetColorSpaceID(AFullColor)] do
    TextOut(Rect.Left+Rect.Bottom-Rect.Top+2,Rect.Top+2,
            Format('%s : %s = $%.2x; %s = $%.2x; %s = $%.2x',
                   [Name,
                   AxisName[TJvAxisIndex(0)], GetAxisValue(AFullColor,TJvAxisIndex(0)),
                   AxisName[TJvAxisIndex(1)], GetAxisValue(AFullColor,TJvAxisIndex(1)),
                   AxisName[TJvAxisIndex(2)], GetAxisValue(AFullColor,TJvAxisIndex(2))]));

    Brush.Color:=ColorSpaceManager.ConvertToColor(AFullColor);
    Pen.Color:=clBlack;
    Rectangle(Rect.Left+2,Rect.Top+2,Rect.Left+Rect.Bottom-Rect.Top-2,Rect.Bottom-2);
  end;
end;

end.
