unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvPrvwDoc, ComCtrls, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    tbCols: TTrackBar;
    tbCount: TTrackBar;
    AutoScroll: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure tbColsChange(Sender: TObject);
    procedure tbCountChange(Sender: TObject);
    procedure AutoScrollClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pd:TJvPreviewDoc;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  pd := TJvPreviewDoc.Create(self);
  pd.Parent := self;
  pd.Align := alClient;
//  pd.Options.DrawMargins := true;
  pd.Options.Count := 1;
  pd.Cols := 1;
end;

procedure TForm1.tbColsChange(Sender: TObject);
begin
  pd.Cols := tbCols.Position;
  tbCols.Hint := Format('Cols: %d',[tbCols.Position]);
end;

procedure TForm1.tbCountChange(Sender: TObject);
begin
  pd.Options.Count := tbCount.Position;
  tbCount.Hint := Format('Count: %d',[tbCount.Position]);
end;

procedure TForm1.AutoScrollClick(Sender: TObject);
begin
  pd.AutoScroll := AutoScroll.Checked;
end;

end.
