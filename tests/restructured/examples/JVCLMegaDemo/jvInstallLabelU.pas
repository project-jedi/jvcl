unit jvInstallLabelU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, JvComponent, JvInstallLabel, ExtCtrls, StdCtrls;

type
  TJvInstallLabelFrm = class(TFrame)
    Image1: TImage;
    Next: TButton;
    Button1: TButton;
    Panel1: TPanel;
    InstallLabel1: TJvInstallLabel;
    ImageList1: TImageList;
    procedure NextClick(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
  private
    { Private declarations }
    FIndex:integer;    
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TJvInstallLabelFrm.NextClick(Sender: TObject);
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

procedure TJvInstallLabelFrm.FrameEnter(Sender: TObject);
begin
  FIndex := 0;
end;

end.
