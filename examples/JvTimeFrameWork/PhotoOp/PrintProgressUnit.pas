unit PrintProgressUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls;

type
  TPrintProgress = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    ProgressBar1: TProgressBar;
    CancelButton: TBitBtn;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PrintProgress: TPrintProgress;

implementation

uses PhotoOpUnit;

{$R *.DFM}

procedure TPrintProgress.CancelButtonClick(Sender: TObject);
begin
  PhotoOpMain.JvTFDaysPrinter1.AbortPrint;
end;

procedure TPrintProgress.FormShow(Sender: TObject);
begin
  ProgressBar1.Position := 0;
  Label2.Caption := 'Processing...';
end;

end.
