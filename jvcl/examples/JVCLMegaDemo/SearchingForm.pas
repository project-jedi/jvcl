unit SearchingForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, JvListView, Buttons, JvBitBtn, JclStrings,
  JvLabel;

type
  TSearchingFormMain = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    ComponentEdit: TEdit;
    SearchButton: TJvBitBtn;
    Label3: TLabel;
    ResultListView: TJvListView;
    StatusLabel: TJvLabel;
    procedure SearchButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ResultListViewDblClick(Sender: TObject);
    procedure ComponentEditChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SearchingFormMain: TSearchingFormMain;


implementation

uses UnitMain;

{$R *.dfm}


procedure TSearchingFormMain.SearchButtonClick(Sender: TObject);
var
  I,K: integer;
  ListItem: TListItem;
  Found : Boolean;
begin
  Found := false;
  try
    Screen.Cursor := crHourglass;

    //check if the forms was created, except itself
    for I := 1 to MAX_FORMS do
    begin
      StatusLabel.Caption := 'Creating Form ' + IntToStr(I) + ' of ' + IntToStr(MAX_FORMS);
      Application.ProcessMessages;
      if (I <> 8) and (TheFormArray[I] = nil) then
        Mainform.CreateDemoForm(I,false); //false is to avoid free previous form
    end;
    Application.ProcessMessages;
    ResultListView.Items.Clear;
    Found := false;
    for I := 1 to MAX_FORMS do
    begin
      StatusLabel.Caption := 'Searching Form ' + IntToStr(I) + ' of ' + IntToStr(MAX_FORMS);
      StatusLabel.Update;
      //find component into the form
      for K := 0 to TheFormArray[I].ComponentCount -1 do
        if StrIPos(ComponentEdit.Text,TheFormArray[I].Components[K].ClassName) > 0 then
        begin
          //Found! - Inserting into ListView
          ListItem := ResultListView.Items.Add;
          ListItem.Caption := IntToStr(I);
          Application.ProcessMessages;
          ListItem.SubItems.Add(TheFormArray[I].ClassName);
          Found := true;
          break;
        end;
    end;
  finally
    Screen.Cursor := crDefault;
    if Found then
      StatusLabel.Caption := 'Double-click on desired form to see the demo'
    else
      StatusLabel.Caption := 'No forms found.';
  end;
end;

procedure TSearchingFormMain.FormShow(Sender: TObject);
begin
  StatusLabel.Caption := '';
  SearchButton.Enabled := false;
  ComponentEdit.Text := '';
end;

procedure TSearchingFormMain.ResultListViewDblClick(Sender: TObject);
begin
  if ResultListView.Selected.Caption <> '8' then
  begin
    TheFormArray[StrToInt(ResultListView.Selected.Caption)].Position := poScreenCenter;
    TheFormArray[StrToInt(ResultListView.Selected.Caption)].ShowModal;
  end
  else
    ShowMessage('It´s me!');
end;

procedure TSearchingFormMain.ComponentEditChange(Sender: TObject);
begin
  if ComponentEdit.Text <> '' then
    SearchButton.Enabled := true
  else
    SearchButton.Enabled := false;
end;

end.

