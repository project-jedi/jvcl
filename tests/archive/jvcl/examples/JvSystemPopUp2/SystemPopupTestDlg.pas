{$I JVCL.INC}
unit SystemPopupTestDlg;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6_UP}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, Menus, JvComponent, JvSystemPopup, ActnList,
  StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    JvSystemPopup1: TJvSystemPopup;
    PopupMenu1: TPopupMenu;
    est1: TMenuItem;
    ActionList1: TActionList;
    actClickToCheck: TAction;
    Action11: TMenuItem;
    PopupMenu2: TPopupMenu;
    Action21: TMenuItem;
    btnSwitch: TButton;
    rgrPosition: TRadioGroup;
    rgrPositionInMenu: TRadioGroup;
    edtAdd: TEdit;
    btnAdd: TButton;
    Radio21: TMenuItem;
    Radio31: TMenuItem;
    Menu21: TMenuItem;
    DisabledItem1: TMenuItem;
    actClickToSwitchIcon: TAction;
    ImageList1: TImageList;
    ClickToSwitchIcon1: TMenuItem;
    chbHideHideableItem: TCheckBox;
    actHideableItem: TAction;
    HideableItem1: TMenuItem;
    HideableItem2: TMenuItem;
    actShortCut: TAction;
    ShortCut1: TMenuItem;
    ShortCut2: TMenuItem;
    procedure actClickToCheckExecute(Sender: TObject);
    procedure btnSwitchClick(Sender: TObject);
    procedure rgrPositionClick(Sender: TObject);
    procedure rgrPositionInMenuClick(Sender: TObject);
    procedure RadioClick(Sender: TObject);
    procedure actClickToSwitchIconExecute(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure chbHideHideableItemClick(Sender: TObject);
    procedure actHideableItemExecute(Sender: TObject);
    procedure actShortCutExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  JvTypes;

{$R *.dfm}

procedure TForm1.actClickToCheckExecute(Sender: TObject);
begin
  if Sender is TAction then
    with Sender as TAction do
      Checked := not Checked;
end;

procedure TForm1.btnSwitchClick(Sender: TObject);
begin
  with JvSystemPopup1 do
    if Popup = PopupMenu1 then
      Popup := PopupMenu2
    else
      Popup := PopupMenu1;
end;

procedure TForm1.rgrPositionClick(Sender: TObject);
begin
  JvSystemPopup1.Position := TPopupPosition(rgrPosition.ItemIndex);
end;

procedure TForm1.rgrPositionInMenuClick(Sender: TObject);
begin
  JvSystemPopup1.PositionInMenu := TPositionInMenu(rgrPositionInMenu.ItemIndex);
end;

procedure TForm1.RadioClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    with Sender as TMenuItem do
      Checked := not Checked;
end;

procedure TForm1.actClickToSwitchIconExecute(Sender: TObject);
begin
  if Sender is TAction then
    with Sender as TAction do
      ImageIndex := ((ImageIndex + 2) mod 4) - 1;
end;

procedure TForm1.btnAddClick(Sender: TObject);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(nil);
  with MenuItem do
  begin
    Caption := edtAdd.Text;
  end;
  PopupMenu1.Items.Items[0].Add(MenuItem);
  if JvSystemPopup1.Popup = PopupMenu1 then
    JvSystemPopup1.Refresh;
end;

procedure TForm1.chbHideHideableItemClick(Sender: TObject);
begin
  actHideableItem.Visible := not chbHideHideableItem.Checked;
end;

procedure TForm1.actHideableItemExecute(Sender: TObject);
begin
  chbHideHideableItem.Checked := True;
end;

procedure TForm1.actShortCutExecute(Sender: TObject);
begin
  ShowMessage('ShortCut');
end;

end.

