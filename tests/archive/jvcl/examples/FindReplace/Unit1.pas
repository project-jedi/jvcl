{ Test program for the FindReplace unit.
  Has two different Find systems: one using the built in dialogs; one using
  InputQuery to handle user selection. See FindReplace.pas / readme.txt for more details
}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, JvFindReplace, JvComponent;

type
  TForm1 = class(TForm)
    FindReplace1: TJvFindReplace;
    Memo1: TMemo;
    MainMenu1: TMainMenu;
    Search1: TMenuItem;
    Find1: TMenuItem;
    Replace1: TMenuItem;
    FindAgain1: TMenuItem;
    Serachown1: TMenuItem;
    Find2: TMenuItem;
    Replace2: TMenuItem;
    FindAgain2: TMenuItem;
    Options1: TMenuItem;
    Rememberlastsearch1: TMenuItem;
    procedure Find1Click(Sender: TObject);
    procedure Replace1Click(Sender: TObject);
    procedure FindAgain1Click(Sender: TObject);
    procedure FindReplace1NotFound(Sender: TObject);
    procedure FindReplace1Find(Sender: TObject);
    procedure FindReplace1Replace(Sender: TObject);
    procedure Find2Click(Sender: TObject);
    procedure Replace2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FindAgain2Click(Sender: TObject);
    procedure FindReplace1Show(Sender: TObject);
    procedure FindReplace1Close(Sender: TObject);
    procedure Rememberlastsearch1Click(Sender: TObject);
  private
    { Private declarations }
    FOptions:TFindOptions;
    FCount:integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Find1Click(Sender: TObject);
begin
  FindReplace1.ShowDialogs := True;
  FindReplace1.Find;
end;

procedure TForm1.Replace1Click(Sender: TObject);
begin
  FindReplace1.ShowDialogs := True;
  FindReplace1.Replace;
end;

procedure TForm1.FindAgain1Click(Sender: TObject);
begin
  { reset to saved options }
  FindReplace1.Options := FOptions;
  FindReplace1.FindAgain;
end;

procedure TForm1.FindReplace1NotFound(Sender: TObject);
begin
  if not FindReplace1.ShowDialogs then
    ShowMessage('Text not found!');
  Caption := 'Not found! ' + IntToStr(FCount);
  Inc(FCount);
end;

procedure TForm1.FindReplace1Find(Sender: TObject);
begin
  FOptions := FindReplace1.Options;
  Caption := 'Find next clicked! ' + IntToStr(FCount);
  Inc(FCount);
end;

procedure TForm1.FindReplace1Replace(Sender: TObject);
begin
  Caption := 'Replace clicked! '  + IntToStr(FCount);
  Inc(FCount);
end;

procedure TForm1.Find2Click(Sender: TObject);
var S:string;
begin
  S := FindReplace1.FindText;
  if InputQuery('Find','Search for:',S) then
  begin
    FindReplace1.ShowDialogs := False;
    FindReplace1.FindText := S;
    FindReplace1.Find;
  end;
end;

procedure TForm1.Replace2Click(Sender: TObject);
var S,R:string;
begin
   S := FindReplace1.FindText;
   R := FindReplace1.ReplaceText;
   if InputQuery('Find','Find text:',S) then
     if InputQuery('Replace','Replace with:',R) then
     begin
       FindReplace1.ShowDialogs := False;
       FindReplace1.FindText := S;
       FindReplace1.ReplaceText := R;
       FindReplace1.Replace;
     end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Fcount := 0;
end;

procedure TForm1.FindAgain2Click(Sender: TObject);
begin
  FindReplace1.FindAgain;
end;

procedure TForm1.FindReplace1Show(Sender: TObject);
begin
  Caption := 'Showing';
end;

procedure TForm1.FindReplace1Close(Sender: TObject);
begin
  Caption := 'Closing';
end;

procedure TForm1.Rememberlastsearch1Click(Sender: TObject);
begin
   Rememberlastsearch1.Checked := not Rememberlastsearch1.Checked;
   FindReplace1.Keeptext := Rememberlastsearch1.Checked;
end;

end.
