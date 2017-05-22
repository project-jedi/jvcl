{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

unit DictMain;

interface

uses
  QWindows, QMessages, SysUtils, Variants, Classes, Types, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls;

type
  TForm1 = class(TForm)
    Compress: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Uncompress: TButton;
    procedure CompressClick(Sender: TObject);
    procedure UncompressClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

uses
  Math;

{$R *.xfm}

procedure TForm1.CompressClick(Sender: TObject);
var
  I: Integer;
  J: Integer;
  S1: string;
  S2: string;
  List1: TStringList;
  List2: TStringList;
  Digit: Integer;
  Ch: Char;
  Stream: TFileStream;
begin
  if OpenDialog1.Execute then
  begin
    List1 := TStringList.Create;
    List2 := TStringList.Create;
    List1.LoadFromFile(OpenDialog1.FileName);
    List1.Sorted := True;
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
    begin
      Ch := #10;
      Stream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
      for I := 0 to List2.Count-1 do
      begin
        Stream.Write(PChar(List2[I])^, Length(List2[I]));
        Stream.Write(Ch, 1);
      end;
      Stream.Free;
    end;
      // List2.SaveToFile(SaveDialog1.FileName);
    List1.Free;
    List2.Free;
  end;
end;

procedure TForm1.UncompressClick(Sender: TObject);
var
  AFile: TextFile;
  Value: string;
  LastValue: string;
  N: Integer;
  List: TStringList;
begin
  List := TStringList.Create;
  if OpenDialog1.Execute then
  begin
    System.AssignFile(AFile, OpenDialog1.FileName);
    System.Reset(AFile);
    try
      repeat
        Readln(AFile, Value);
        if Value <> '' then
        begin
          // (rom) simple compession for dictionary
          N := Ord(Value[1]) - Ord('0');
          Value := Copy(Value, 2, Length(Value) - 1);
          if N > 0 then
            Value := Copy(LastValue, 1, N) + Value;
          LastValue := Value;
          List.Add(Value);
        end;
      until Eof(AFile);
    finally
      System.Close(AFile);
    end;
    if SaveDialog1.Execute then
      List.SaveToFile(SaveDialog1.FileName);
  end;
  List.Free;
end;

end.
