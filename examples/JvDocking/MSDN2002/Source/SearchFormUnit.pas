unit SearchFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls;

type
  TSearchForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    LookforComboBox: TComboBox;
    FilteredbyComboBox: TComboBox;
    lbDockClient1: TJvDockClient;
    SearchButton: TButton;
    SearchintitlesonlyCheckBox: TCheckBox;
    MatchrelatedwordsCheckBox: TCheckBox;
    SearchinpreviousresultsCheckBox: TCheckBox;
    HighlightsearchhitsCheckBox: TCheckBox;
    procedure Panel1Resize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SearchForm: TSearchForm;

implementation

uses MSDN2002MainUnit;

{$R *.DFM}

procedure TSearchForm.Panel1Resize(Sender: TObject);
begin
  LookforComboBox.Width := Panel1.ClientWidth - 0;
  FilteredbyComboBox.Width := Panel1.ClientWidth - 0;
end;

procedure TSearchForm.FormCreate(Sender: TObject);
begin
  FilteredbyComboBox.ItemIndex := 0;
end;

end.
