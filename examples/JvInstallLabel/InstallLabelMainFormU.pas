unit InstallLabelMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ImgList, JvInstallLabel, JvComponent;

type
  TInstallLabelMainForm = class(TForm)
    ImageList1: TImageList;
    Next: TButton;
    Button1: TButton;
    Image1: TImage;
    Panel1: TPanel;
    InstallLabel1: TJvInstallLabel;
    procedure NextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FIndex:integer;
  end;

var
  InstallLabelMainForm: TInstallLabelMainForm;

implementation

{$R *.DFM}

procedure TInstallLabelMainForm.NextClick(Sender: TObject);
begin
  Next.Caption := '&Next >>';
  if FIndex = 0 then { first line, so clear all others }
    InstallLabel1.SetExclusive(FIndex,0,[fsBold])
  else
  begin
    InstallLabel1.SetStyle(FIndex,0,[fsBold]); { current line is an arrow }
    InstallLabel1.SetStyle(FIndex - 1,1,[]); { prev line is a check mark }

    if FIndex = InstallLabel1.Lines.Count - 1 then { this is the last line }
    begin
      FIndex := -1; { incremented below...}
      Next.Caption := '&Again...';
    end;
  end;

  Inc(FIndex);
end;

procedure TInstallLabelMainForm.FormCreate(Sender: TObject);
begin
  FIndex := 0;
end;


procedure TInstallLabelMainForm.Button1Click(Sender: TObject);
begin
  Close;
end;





end.
 
