unit DictMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Work: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure WorkClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

uses
  Math;

{$R *.dfm}

procedure TForm1.WorkClick(Sender: TObject);
var
  I: Integer;
  J: Integer;
  S1: string;
  S2: string;
  List1: TStringList;
  List2: TStringList;
  Digit: Integer;
begin
  if OpenDialog1.Execute then
  begin
    List1 := TStringList.Create;
    List2 := TStringList.Create;
    List1.LoadFromFile(OpenDialog1.FileName);
    List2.Add('0' + List1[0]);
    for I := 1 to List1.Count-1 do
    begin
      S1 := List1[I-1];
      S2 := List1[I];
      if S2 <> '' then
      begin
        Digit := 0;
        for J := 1 to Min(9, Min(Length(S1), Length(S2)))+1 do
        begin
          if S1[J] <> S2[J] then
          begin
            Digit := J - 1;
            Break;
          end;
          if J = 10 then
            Digit := 9;
        end;
        List2.Add(Char(Ord('0') + Digit) + Copy(S2, Digit+1, Length(S2)));
      end;
    end;
    if SaveDialog1.Execute then
      List2.SaveToFile(SaveDialog1.FileName);
    List1.Free;
    List2.Free;
  end;
end;

end.
