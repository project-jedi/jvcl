unit JvWizardDemoMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JvWizard, JvWizardRouteMapList, JvExControls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    JvWizard1: TJvWizard;
    JvWizardRouteMapList1: TJvWizardRouteMapList;
    JvWizardWelcomePage: TJvWizardWelcomePage;
    JvWizardInteriorPage1: TJvWizardInteriorPage;
    JvWizardInteriorPage2: TJvWizardInteriorPage;
    JvWizardFinalPage: TJvWizardInteriorPage;
    ContinueCheckBox: TCheckBox;
    JvWizardInteriorPage3: TJvWizardInteriorPage;
    SkipCheckBox: TCheckBox;
    EnterEdit: TEdit;
    Label1: TLabel;
    procedure ContinueCheckBoxClick(Sender: TObject);
    procedure JvWizardFinalPageFinishButtonClick(Sender: TObject; var Stop: Boolean);
    procedure JvWizardInteriorPage1NextButtonClick(Sender: TObject; var Stop: Boolean);
    procedure JvWizardInteriorPage2EnterPage(Sender: TObject; const FromPage: TJvWizardCustomPage);
  private
    { Private declarations }
  protected
    procedure CalculateButtons;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.CalculateButtons;
begin
  JvWizardInteriorPage2.EnableButton(bkNext, ContinueCheckBox.Checked);
  JvWizardInteriorPage3.Enabled := not SkipCheckBox.Checked;
end;

procedure TForm1.ContinueCheckBoxClick(Sender: TObject);
begin
  CalculateButtons;
end;

procedure TForm1.JvWizardFinalPageFinishButtonClick(Sender: TObject; var Stop: Boolean);
begin
  Close;
end;

procedure TForm1.JvWizardInteriorPage1NextButtonClick(Sender: TObject; var Stop: Boolean);
begin
  if trim(EnterEdit.Text) =  '' then
  begin
    MessageDlg('You have to enter something into the edit field.', mtError, [mbOK], 0);
    Stop := true;
  end;
end;

procedure TForm1.JvWizardInteriorPage2EnterPage(Sender: TObject; const FromPage: TJvWizardCustomPage);
begin
  CalculateButtons;
end;

end.
