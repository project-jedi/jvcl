// example of how to use the HexDump component
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TipWin, Menus, HexDump,  ExtCtrls;

type
  TForm1 = class(TForm)
    TipWindow1: TTipWindow;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    TipoftheDay1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    HexDump1: THexDump;
    OpenDialog1: TOpenDialog;
    procedure Exit1Click(Sender: TObject);
    procedure TipoftheDay1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  FDump:TMemoryStream;
implementation

{$R *.DFM}

procedure TForm1.Exit1Click(Sender: TObject);
begin
  FDump.Free;
  Close;
end;

procedure TForm1.TipoftheDay1Click(Sender: TObject);
var S:string;
begin
  S := 'Loadme.txt';
  if FileExists(S) then
    TipWindow1.LoadFromFile(S);

  if TipWindow1.Execute then
     ShowMessage('Sure, I''ll show the tips the next time too.');
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    if FDump <> nil then
    begin
      with HexDump1 do
      begin
        Address := Nil;
        CurrentLine := -1;
        DataSize := 0;
      end;
      FDump.Free;
      FDump := Nil;
    end;
    FDump := TMemoryStream.Create;
  try
    FDump.LoadFromFile(OpenDialog1.FileName);
    OpenDialog1.HistoryList.Add(OpenDialog1.Filename);
    with HexDump1 do
    begin
      Address := FDump.Memory;
      CurrentLine := 0;
      DataSize := FDump.Size;
    end;
  except
    FDump.Free;
    FDump := nil;
  end;
  end;
end;

end.
