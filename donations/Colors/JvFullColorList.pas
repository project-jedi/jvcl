unit JvFullColorList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ColorSpaces, FullColorFrm, StdCtrls, ColorDialogs, ActnList,
  Buttons, ImgList;

type
  TFormFullColorList = class(TForm)
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
    procedure ActionNewUpdate(Sender: TObject);
    procedure ActionModifyUpdate(Sender: TObject);
    procedure ActionDeleteUpdate(Sender: TObject);
    procedure ActionMoveUpUpdate(Sender: TObject);
    procedure ActionMoveDownUpdate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormFullColorList: TFormFullColorList;

implementation

{$R *.dfm}

procedure TFormFullColorList.ActionNewUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=True;
end;

procedure TFormFullColorList.ActionModifyUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=ListBoxColors.SelCount=1;
end;

procedure TFormFullColorList.ActionDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=ListBoxColors.SelCount>=1;
end;

procedure TFormFullColorList.ActionMoveUpUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=
        (ListBoxColors.SelCount=1) and (not ListBoxColors.Selected[0]);
end;

procedure TFormFullColorList.ActionMoveDownUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(ListBoxColors.SelCount=1)
          and (not ListBoxColors.Selected[ListBoxColors.Count-1]);
end;

end.
